package com.sap.gtt.v2.sample.pof.constant;

import static com.sap.gtt.v2.sample.pof.exception.POFServiceException.MESSAGE_CODE_UNSUPPORTED_TRACKING_ID_TYPE;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import java.util.Arrays;
import org.springframework.http.HttpStatus;

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
                .orElseThrow(() -> new POFServiceException(MESSAGE_CODE_UNSUPPORTED_TRACKING_ID_TYPE, new Object[]{trackingIdType},
                    HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }
}
