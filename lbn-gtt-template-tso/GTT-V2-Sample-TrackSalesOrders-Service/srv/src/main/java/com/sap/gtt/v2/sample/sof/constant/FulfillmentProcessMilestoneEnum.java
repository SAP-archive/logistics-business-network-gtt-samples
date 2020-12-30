package com.sap.gtt.v2.sample.sof.constant;

public enum FulfillmentProcessMilestoneEnum {
    SALES_ORDER_ITEM_CREATED("salesOrderItemCreated", 0),
    SALES_ORDER_ITEM_CONFIRMED("salesOrderItemConfirmed", 1),
    DELIVERY_CREATED("deliveryCreated", 2),
    DELIVERY_GOODS_ISSUED("deliveryGoodsIssued", 3),
    DELIVERY_COMPLETED("deliveryCompleted", 4),
    SALES_ORDER_ITEM_REJECTED("salesOrderItemRejected", 1);

    private final String name;
    private final Integer position;

    FulfillmentProcessMilestoneEnum(String name, Integer position) {
        this.name = name;
        this.position = position;
    }

    public String getName() {
        return name;
    }

    public Integer getPosition() {
        return position;
    }
}
