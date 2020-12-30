package com.sap.gtt.v2.sample.pof.constant;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;

import java.util.Arrays;

import static java.lang.String.format;

public enum TrackingIdTypeEnum {
    PURCHASE_ORDER("PURCHASE_ORDER"),
    PURCHASE_ORDER_ITEM("PURCHASE_ORDER_ITEM"),
    INBOUND_DELIVERY_ITEM("INBOUND_DELIVERY_IT"),
    INBOUND_DELIVERY("INBOUND_DELIVERY"),
    SHIPMENT("INBOUND_SHIPMENT"),
    RESOURCE("INBOUND_RESOURCE");

    private final String trackingIdType;

    TrackingIdTypeEnum(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public static TrackingIdTypeEnum getTrackingIdTypeEnum(String trackingIdType) {
        return Arrays.stream(values())
                .filter(type -> type.getTrackingIdType().equals(trackingIdType))
                .findFirst()
                .orElseThrow(() -> new POFServiceException(format("Unsupported trackingIdType: %s", trackingIdType)));
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }
}
