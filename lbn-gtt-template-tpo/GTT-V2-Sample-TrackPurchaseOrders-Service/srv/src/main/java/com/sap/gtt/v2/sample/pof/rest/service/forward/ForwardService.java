package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.EventEx;
import com.sap.gtt.v2.sample.pof.domain.InboundDeliveryItemEvent;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderItemEvent;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ForwardUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.Constants.DELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.Constants.UNDELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.INBOUND_DELIVERY_ITEM;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.PURCHASE_ORDER_ITEM;
import static com.sap.gtt.v2.sample.pof.exception.POFServiceException.MESSAGE_CODE_ERROR_NO_DATA_FOUND;
import static com.sap.gtt.v2.sample.pof.service.MapService.ALT_KEY;
import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Service
public class ForwardService {

    private static final Logger logger = LoggerFactory.getLogger(ForwardService.class);

    private static final String PURCHASE_ORDER_URI = "/PurchaseOrder";
    private static final String QUERY_TEMPLATE = "$filter=purchaseOrderItemTPs/id eq guid'%s'" +
            "&$expand=purchaseOrderItemTPs/inboundDeliveryItems";
    private static final String TRACKED_PROCESS = "trackedProcess";
    private static final String TRACKING_ID_TYPE = "trackingIdType";
    private static final String PURCHASE_ORDER_ITEM_ID = "purchaseOrderItem_id";
    private static final String ACTUAL_EVENT = "actualEvent";
    private static final String EVENT_TYPE = "eventType";
    private static final String PROCESS_ID = "id";
    private static final String DELIVERY_NO = "inboundDeliveryNo";
    private static final String ITEM_NO = "itemNo";
    private static final String LAST_CORRELATED_EVENT_ID = "lastCorrelatedEventId";
    public static final String ID = "id";
    public static final String INBOUND_DELIVERY_ITEM_EVENT = "/InboundDeliveryItemEvent";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;
    @Autowired
    private InternalExecutionStatusService internalExecutionStatusService;
    @Autowired
    private InternalCompleteValueService internalCompleteValueService;
    @Autowired
    private ForwardConverter forwardConverter;

    public void execute(String body) {
        JsonObject jsonObject = JsonParser.parseString(body).getAsJsonObject();
        JsonObject trackedProcess = jsonObject.getAsJsonObject(TRACKED_PROCESS);
        if (!trackedProcess.has(TRACKING_ID_TYPE)) {
            return;
        }

        JsonObject actualEvent = jsonObject.getAsJsonObject(ACTUAL_EVENT);
        String eventType = actualEvent.get(EVENT_TYPE).getAsString();

        PurchaseOrder purchaseOrder;
        Optional<InboundDeliveryItem> deliveryItemOptional = Optional.empty();

        String trackingIdType = trackedProcess.get(TRACKING_ID_TYPE).getAsString();
        if (INBOUND_DELIVERY_ITEM.getTrackingIdType().equals(trackingIdType)) {
            if (POFUtils.isEventTypeInWhiteList(eventType)) {
                updateLastActivityOfDeliveryItem(trackedProcess);
            }

            String purchaseOrderItemId = trackedProcess.get(PURCHASE_ORDER_ITEM_ID).getAsString();
            String inboundDeliveryId = trackedProcess.get(PROCESS_ID).getAsString();
            purchaseOrder = fetchPurchaseOrder(purchaseOrderItemId);
            deliveryItemOptional = getInboundDeliveryItem(purchaseOrder, inboundDeliveryId);
        } else if (PURCHASE_ORDER_ITEM.getTrackingIdType().equals(trackingIdType)) {
            String purchaseOrderId = trackedProcess.get(PROCESS_ID).getAsString();
            purchaseOrder = fetchPurchaseOrder(purchaseOrderId);
        } else {
            // eventType doesn't need additional calculations
            return;
        }
        processInboundDeliveryItemProcess(deliveryItemOptional, eventType);
        processPurchaseOrderItemProcess(purchaseOrder, eventType);
    }

    public void updateLastActivityOfDeliveryItem(JsonObject trackedProcess) {
        String lastCorrelatedEventId = trackedProcess.get(LAST_CORRELATED_EVENT_ID).getAsString();
        String tpId = trackedProcess.get(ID).getAsString();
        if (StringUtils.isEmpty(lastCorrelatedEventId) || StringUtils.isEmpty(tpId)) {
            return;
        }

        String query = UriComponentsBuilder.fromUriString("/Event(guid'" + lastCorrelatedEventId + "')")
                .build().encode().toUriString();

        Event event = gttCoreServiceClient.readEntity(query, Event.class);
        String eventType = event.getEventType().substring(event.getEventType().lastIndexOf(".")+1);
        query = UriComponentsBuilder.fromUriString("/" + eventType + "(guid'" + lastCorrelatedEventId + "')")
                .queryParam(EXPAND, "eventProcesses/plannedEvent")
                .build().encode().toUriString();

        EventEx actualEvent = gttCoreServiceClient.readEntity(query, EventEx.class);
        PlannedEvent plannedEvent = null;
        for (ProcessEventDirectory ped : actualEvent.getEventProcesses()) {
            if (UUID.fromString(tpId).equals(ped.getProcessId())) {
                plannedEvent = ped.getPlannedEvent();
                break;
            }
        }

        String lastEventName, lastVPLocationTypeCode , lastLocationAltKey = null;
        lastVPLocationTypeCode = actualEvent.getLocationTypeCode();
        if (plannedEvent != null) {
            lastLocationAltKey = plannedEvent.getLocationAltKey();
        } else if (isNotBlank(actualEvent.getLocationAltKey())) {
            lastLocationAltKey = actualEvent.getLocationAltKey();
        } else {
            lastLocationAltKey = EMPTY;
        }

        String eventName = actualEvent.getEventType();
        lastEventName = eventName.substring(eventName.lastIndexOf(".") + 1);

        String altKey = trackedProcess.get(ALT_KEY).getAsString();
        String deliveryNo = trackedProcess.get(DELIVERY_NO).getAsString();
        String itemNo = trackedProcess.get(ITEM_NO).getAsString();

        updateLastActivityOfDeliveryItem(lastEventName, lastVPLocationTypeCode, lastLocationAltKey, altKey, deliveryNo, itemNo);
    }

    public void updateLastActivityOfDeliveryItem(String lastEventName, String lastVPLocationTypeCode, String lastLocationAltKey,
                                                 String altKey, String deliveryNo, String itemNo) {
        InboundDeliveryItemEvent deliveryItemEvent = forwardConverter
                .generateDeliveryItemEvent(altKey, deliveryNo, itemNo, lastEventName, lastVPLocationTypeCode, lastLocationAltKey);
        String body = new Gson().toJson(deliveryItemEvent);
        gttCoreServiceClient.write(body, INBOUND_DELIVERY_ITEM_EVENT);
    }

    private void processInboundDeliveryItemProcess(Optional<InboundDeliveryItem> deliveryItemOptional, String eventType) {
        if (!deliveryItemOptional.isPresent() || !POFUtils.isEventTypesEqual(Constants.GOODS_RECEIPT_EVENT, eventType)) {
            return;
        }
        InboundDeliveryItem inboundDeliveryItem = deliveryItemOptional.get();
        logger.info("Process inboundDelivery with id {}", inboundDeliveryItem.getId());

        String oldExecutionStatusCode = inboundDeliveryItem.getExecutionStatusCode();

        internalExecutionStatusService.updateForNotPODInboundDelivery(inboundDeliveryItem);

        if (!oldExecutionStatusCode.equals(inboundDeliveryItem.getExecutionStatusCode())) {
            write(inboundDeliveryItem);
        }
    }

    private void processPurchaseOrderItemProcess(PurchaseOrder purchaseOrder, String eventType) {
        if (!ForwardUtils.isActualEventForCompletion(eventType) && !isDeletionUndeletionEvent(eventType)) {
            return;
        }
        logger.info("Process purchaseOrder with id {}", purchaseOrder.getId());

        BigDecimal purchaseOrderCompletionValue = isNull(purchaseOrder.getCompletionValue()) ? BigDecimal.ZERO : purchaseOrder.getCompletionValue();
        Map<UUID, BigDecimal> oldCompletionValues = getCompletionValues(purchaseOrder);

        internalCompleteValueService.recalculateCompletionValue(purchaseOrder);

        writeOnlyUpdatedPOItems(purchaseOrder.getPurchaseOrderItemTPs(), oldCompletionValues);
        if (purchaseOrderCompletionValue.compareTo(purchaseOrder.getCompletionValue()) != 0) {
            write(purchaseOrder);
        }
    }

    private Map<UUID, BigDecimal> getCompletionValues(PurchaseOrder purchaseOrder) {
        return purchaseOrder.getPurchaseOrderItemTPs().stream()
                .collect(Collectors.toMap(PurchaseOrderItem::getId, it -> {
                    BigDecimal completionValue = it.getCompletionValue();
                    return Optional.ofNullable(completionValue).orElse(BigDecimal.ZERO);
                }));
    }

    private boolean isDeletionUndeletionEvent(String eventType) {
        return !isBlank(eventType) && (eventType.endsWith(DELETION_EVENT_ENTITY_NAME) || eventType.endsWith(UNDELETION_EVENT_ENTITY_NAME));
    }

    private Optional<InboundDeliveryItem> getInboundDeliveryItem(PurchaseOrder purchaseOrder, String trackedProcess) {
        return purchaseOrder.getPurchaseOrderItemTPs().stream()
                .flatMap(item -> item.getInboundDeliveryItems().stream())
                .filter(Objects::nonNull)
                .filter(inboundDeliveryItem -> inboundDeliveryItem.getId().toString().equals(trackedProcess))
                .findFirst();
    }

    private PurchaseOrder fetchPurchaseOrder(String purchaseOrderItemId) {
        String url = UriComponentsBuilder.fromUriString(PURCHASE_ORDER_URI)
                .query(String.format(QUERY_TEMPLATE, purchaseOrderItemId))
                .encode().toUriString();
        List<PurchaseOrder> results = gttCoreServiceClient.readEntitySetAll(url, PurchaseOrder.class).getResults();
        if (results.isEmpty()) {
            throw new POFServiceException(MESSAGE_CODE_ERROR_NO_DATA_FOUND);
        }
        return results.get(0);
    }

    private void write(InboundDeliveryItem inboundDeliveryItem) {
        InboundDeliveryItemEvent inboundDeliveryItemEvent = forwardConverter.convertInboundDeliveryItem(inboundDeliveryItem);
        String json = new Gson().toJson(inboundDeliveryItemEvent);
        gttCoreServiceClient.write(json, INBOUND_DELIVERY_ITEM_EVENT);
    }

    private void write(PurchaseOrder purchaseOrder) {
        PurchaseOrderEvent purchaseOrderEvent = forwardConverter.convertPurchaseOrder(purchaseOrder);
        gttCoreServiceClient.write(new Gson().toJson(purchaseOrderEvent), "/PurchaseOrderEvent");
    }

    private void writeOnlyUpdatedPOItems(List<PurchaseOrderItem> purchaseOrderItems, Map<UUID, BigDecimal> oldValues) {
        Gson gson = new Gson();
        for (PurchaseOrderItem purchaseOrderItem : purchaseOrderItems) {
            BigDecimal oldCompletionValue = oldValues.getOrDefault(purchaseOrderItem.getId(), BigDecimal.ZERO);
            if (oldCompletionValue.compareTo(purchaseOrderItem.getCompletionValue()) != 0) {
                PurchaseOrderItemEvent event = forwardConverter.convertPurchaseOrderItem(purchaseOrderItem);
                gttCoreServiceClient.write(gson.toJson(event), "/PurchaseOrderItemEvent");
            }
        }
    }
}
