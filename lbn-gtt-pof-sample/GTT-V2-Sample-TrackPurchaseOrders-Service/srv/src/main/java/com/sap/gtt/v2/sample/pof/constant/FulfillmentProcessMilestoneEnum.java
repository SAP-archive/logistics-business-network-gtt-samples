package com.sap.gtt.v2.sample.pof.constant;

import java.util.Arrays;

import static org.apache.commons.lang3.StringUtils.EMPTY;

public enum FulfillmentProcessMilestoneEnum {
    PURCHASE_ORDER_ITEM_CREATED("purchaseOrderItemCreated", 0),
    PURCHASE_ORDER_ITEM_CONFIRMED("purchaseOrderItemConfirmed", 1, "com.lbngttsamples.gtt.app.pof.PurchaseOrderItem.ConfirmationEvent"),
    PURCHASE_ORDER_ITEM_RECEIPT("purchaseOrderItemReceipt", 2, "com.lbngttsamples.gtt.app.pof.PurchaseOrderItem.GoodsReceipt"),
    PURCHASE_ORDER_ITEM_DELETED("purchaseOrderItemDeleted", 3),
    INBOUND_DELIVERY_ITEM_CREATED("inboundDeliveryItemCreated", 4),
    INBOUND_DELIVERY_ITEM_RECEIPT("inboundDeliveryItemReceipt", 5),
    INBOUND_DELIVERY_ITEM_COMPLETED("inboundDeliveryItemCompleted", 6);

    private final String name;
    private final Integer order;
    private final String eventType;

    FulfillmentProcessMilestoneEnum(String name, Integer order) {
        this(name, order, EMPTY);
    }

    FulfillmentProcessMilestoneEnum(String name, Integer order, String eventType) {
        this.name = name;
        this.order = order;
        this.eventType = eventType;
    }

    public static FulfillmentProcessMilestoneEnum getMilestoneByName(String name) {
        return Arrays.stream(values())
                .filter(m -> m.getName().equals(name))
                .findFirst()
                .orElseThrow(() -> new IllegalStateException(String.format("Unsupported milestone %s.", name)));
    }

    public String getName() {
        return name;
    }

    public Integer getOrder() {
        return order;
    }

    public String getEventType() {
        return eventType;
    }
}
