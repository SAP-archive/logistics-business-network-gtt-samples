package com.sap.gtt.v2.sample.pof.constant;

public enum DocumentFlowGroupEnum {
    PURCHASE_ORDER(1, "purchaseOrder"),
    PURCHASE_ORDER_ITEM(2, "purchaseOrderItem"),
    INBOUND_DELIVERY_ITEM(3, "inboundDeliveryItem"),
    INBOUND_DELIVERY(4, "inboundDelivery"),
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
