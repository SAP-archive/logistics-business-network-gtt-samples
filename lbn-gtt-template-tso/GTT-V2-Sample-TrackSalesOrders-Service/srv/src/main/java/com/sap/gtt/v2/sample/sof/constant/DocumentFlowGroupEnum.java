package com.sap.gtt.v2.sample.sof.constant;

public enum DocumentFlowGroupEnum {
    SALES_ORDER(1, "salesOrder"),
    SALES_ORDER_ITEM(2, "salesOrderItem"),
    DELIVERY_ITEM(3, "deliveryItem"),
    DELIVERY(4, "delivery"),
    SHIPMENT(5, "shipment"),
    RESOURCE(6, "resource");

    private final Integer groupKey;
    private final String groupTitle;

    DocumentFlowGroupEnum(Integer groupKey, String groupTitle) {
        this.groupKey = groupKey;
        this.groupTitle = groupTitle;
    }

    public Integer getGroupKey() {
        return groupKey;
    }

    public String getGroupTitle() {
        return groupTitle;
    }
}
