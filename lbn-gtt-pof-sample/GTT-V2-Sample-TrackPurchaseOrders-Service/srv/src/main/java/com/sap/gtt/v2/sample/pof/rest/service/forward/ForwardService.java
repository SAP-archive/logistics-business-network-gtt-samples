package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.InboundDeliveryItemEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderItemEvent;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemTP;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ForwardUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import static com.sap.gtt.v2.sample.pof.constant.Constants.UNDELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.INBOUND_DELIVERY_ITEM;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.PURCHASE_ORDER_ITEM;
import static com.sap.gtt.v2.sample.pof.exception.POFServiceException.MESSAGE_CODE_ERROR_NO_DATA_FOUND;
import static java.lang.String.format;
import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.isBlank;

@Service
public class ForwardService {

    private static final Logger logger = LoggerFactory.getLogger(ForwardService.class);

    private static final String PURCHASE_ORDER_URI = "/PurchaseOrder";
    private static final String QUERY_TEMPLATE = "$filter=purchaseOrderItemTPs/purchaseOrderItem_id eq guid'%s'" +
            "&$expand=purchaseOrderItemTPs/purchaseOrderItem/inboundDeliveryItems/inboundDeliveryItem";
    private static final String GOODS_RECEIPT_EVENT =
            format("%s.%s.GoodsReceipt", Constants.GTT_MODEL_NAMESPACE, PurchaseOrderItem.ENTITY_SET_NAME);
    private static final String TRACKED_PROCESS = "trackedProcess";
    private static final String TRACKING_ID_TYPE = "trackingIdType";
    private static final String PURCHASE_ORDER_ITEM_ID = "purchaseOrderItem_id";
    private static final String ACTUAL_EVENT = "actualEvent";
    private static final String EVENT_TYPE = "eventType";
    private static final String PROCESS_ID = "id";

    private final GTTCoreServiceClient gttCoreServiceClient;
    private final InternalExecutionStatusService internalExecutionStatusService;
    private final InternalCompleteValueService internalCompleteValueService;
    private final ForwardConverter forwardConverter;

    public ForwardService(GTTCoreServiceClient gttCoreServiceClient,
                          InternalExecutionStatusService internalExecutionStatusService,
                          InternalCompleteValueService internalCompleteValueService,
                          ForwardConverter forwardConverter) {
        this.gttCoreServiceClient = gttCoreServiceClient;
        this.internalExecutionStatusService = internalExecutionStatusService;
        this.internalCompleteValueService = internalCompleteValueService;
        this.forwardConverter = forwardConverter;
    }

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

    private void processInboundDeliveryItemProcess(Optional<InboundDeliveryItem> deliveryItemOptional, String eventType) {
        if (!deliveryItemOptional.isPresent() || !GOODS_RECEIPT_EVENT.equals(eventType)) {
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

        writeOnlyUpdatedPOItems(getPurchaseOrderItems(purchaseOrder), oldCompletionValues);
        if (purchaseOrderCompletionValue.compareTo(purchaseOrder.getCompletionValue()) != 0) {
            write(purchaseOrder);
        }
    }

    private Map<UUID, BigDecimal> getCompletionValues(PurchaseOrder purchaseOrder) {
        return getPurchaseOrderItems(purchaseOrder).stream()
                .collect(Collectors.toMap(PurchaseOrderItem::getId, it -> {
                    BigDecimal completionValue = it.getCompletionValue();
                    return Optional.ofNullable(completionValue).orElse(BigDecimal.ZERO);
                }));
    }

    private List<PurchaseOrderItem> getPurchaseOrderItems(PurchaseOrder purchaseOrder) {
        return purchaseOrder.getPurchaseOrderItemTPs().stream()
                .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private boolean isDeletionUndeletionEvent(String eventType) {
        return !isBlank(eventType) && (eventType.endsWith(DELETION_EVENT_ENTITY_NAME) || eventType.endsWith(UNDELETION_EVENT_ENTITY_NAME));
    }

    private Optional<InboundDeliveryItem> getInboundDeliveryItem(PurchaseOrder purchaseOrder, String trackedProcess) {
        return getPurchaseOrderItems(purchaseOrder).stream()
                .flatMap(item -> item.getInboundDeliveryItems().stream())
                .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
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
        gttCoreServiceClient.write(json, "/InboundDeliveryItemEvent");
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
