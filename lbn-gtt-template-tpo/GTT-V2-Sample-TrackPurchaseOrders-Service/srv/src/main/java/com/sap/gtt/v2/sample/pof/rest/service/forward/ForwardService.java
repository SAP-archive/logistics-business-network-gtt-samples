package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sap.gtt.v2.sample.pof.domain.InboundDeliveryItemEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderItemEvent;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ForwardUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.INBOUND_DELIVERY_ITEM;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.PURCHASE_ORDER_ITEM;
import static com.sap.gtt.v2.sample.pof.exception.POFServiceException.MESSAGE_CODE_ERROR_NO_DATA_FOUND;

@Service
public class ForwardService {

    private static final String PURCHASE_ORDER_URI = "/PurchaseOrder";
    private static final String QUERY_TEMPLATE = "$filter=purchaseOrderItemTPs/id eq guid'%s'" +
            "&$expand=purchaseOrderItemTPs/inboundDeliveryItems";
    private static final String TRACKED_PROCESS = "trackedProcess";
    private static final String TRACKING_ID_TYPE = "trackingIdType";
    private static final String PURCHASE_ORDER_ITEM_ID = "purchaseOrderItem_id";
    private static final String ACTUAL_EVENT = "actualEvent";
    private static final String EVENT_TYPE = "eventType";
    private static final String PROCESS_ID = "id";
    public static final String ID = "id";
    public static final String INBOUND_DELIVERY_ITEM_EVENT = "/InboundDeliveryItemEvent";
    public static final String PURCHASE_ORDER_EVENT = "/PurchaseOrderEvent";
    public static final String PURCHASE_ORDER_ITEM_EVENT = "/PurchaseOrderItemEvent";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;
    @Autowired
    private ForwardConverter forwardConverter;
    @Autowired
    private ForwardFacadeService facadeService;

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

        updateProcess(purchaseOrder, deliveryItemOptional, eventType, trackedProcess);
    }

    private void updateProcess(PurchaseOrder purchaseOrder, Optional<InboundDeliveryItem> deliveryItemOptional, String eventType, JsonObject trackedProcess) {
        PurchaseOrder oldOrder = ForwardUtils.cloneForUpdatable(purchaseOrder);
        Map<UUID, PurchaseOrderItem> oldItemsById = ForwardUtils.cloneOrderItemsAndGroupForUpdatable(purchaseOrder.getPurchaseOrderItemTPs());
        Map<UUID, InboundDeliveryItem> oldDeliveriesById = ForwardUtils.cloneDeliveryItemsAndGroupForUpdatable(getDeliveryItems(purchaseOrder));

        facadeService.updateProperties(purchaseOrder, deliveryItemOptional, eventType);
        deliveryItemOptional.ifPresent(it -> {
            if (POFUtils.isEventTypeInWhiteList(eventType)) {
                facadeService.updateLastActivity(trackedProcess, it);
            }
        });

        getDeliveryItems(purchaseOrder).stream()
                .filter(it -> !ForwardUtils.isUpdatableEqual(it, oldDeliveriesById.get(it.getId())))
                .forEach(this::write);
        purchaseOrder.getPurchaseOrderItemTPs().stream()
                .filter(it -> !ForwardUtils.isUpdatableEqual(it, oldItemsById.get(it.getId())))
                .forEach(this::write);
        if (!ForwardUtils.isUpdatableEqual(oldOrder, purchaseOrder)) {
            write(purchaseOrder);
        }
    }

    private List<InboundDeliveryItem> getDeliveryItems(PurchaseOrder purchaseOrder) {
        return purchaseOrder.getPurchaseOrderItemTPs().stream()
                .flatMap(it -> it.getInboundDeliveryItems().stream())
                .collect(Collectors.toList());
    }

    private Optional<InboundDeliveryItem> getInboundDeliveryItem(PurchaseOrder purchaseOrder, String deliveryItemId) {
        return purchaseOrder.getPurchaseOrderItemTPs().stream()
                .flatMap(item -> item.getInboundDeliveryItems().stream())
                .filter(Objects::nonNull)
                .filter(inboundDeliveryItem -> inboundDeliveryItem.getId().toString().equals(deliveryItemId))
                .findFirst();
    }

    private PurchaseOrder fetchPurchaseOrder(String purchaseOrderItemId) {
        String url = UriComponentsBuilder.fromUriString(PURCHASE_ORDER_URI)
                .query(String.format(QUERY_TEMPLATE, purchaseOrderItemId))
                .encode().toUriString();
        List<PurchaseOrder> results = gttCoreServiceClient.readEntitySetAll(url, PurchaseOrder.class).getResults();
        if (results.isEmpty()) {
            throw new POFServiceException(MESSAGE_CODE_ERROR_NO_DATA_FOUND, HttpStatus.NOT_FOUND.value());
        }
        return results.get(0);
    }

    private void write(InboundDeliveryItem inboundDeliveryItem) {
        InboundDeliveryItemEvent inboundDeliveryItemEvent = forwardConverter.convertInboundDeliveryItem(inboundDeliveryItem);
        String json = POFUtils.getGson().toJson(inboundDeliveryItemEvent);
        gttCoreServiceClient.write(json, INBOUND_DELIVERY_ITEM_EVENT);
    }

    private void write(PurchaseOrder purchaseOrder) {
        PurchaseOrderEvent purchaseOrderEvent = forwardConverter.convertPurchaseOrder(purchaseOrder);
        gttCoreServiceClient.write(POFUtils.getGson().toJson(purchaseOrderEvent), PURCHASE_ORDER_EVENT);
    }

    private void write(PurchaseOrderItem purchaseOrderItem) {
        PurchaseOrderItemEvent event = forwardConverter.convertPurchaseOrderItem(purchaseOrderItem);
        gttCoreServiceClient.write(POFUtils.getGson().toJson(event), PURCHASE_ORDER_ITEM_EVENT);
    }
}
