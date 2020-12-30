package com.sap.gtt.v2.sample.sof.domain;

import com.google.gson.annotations.SerializedName;

public class DeliveryItemEvent extends EventForWrite {
    private String deliveryNo;
    private String itemNo;
    private String lastEventName;
    private String lastLocationAltKey;

    @SerializedName("lastVPLocationType_code")
    private String lastVPLocationTypeCode;

    public String getDeliveryNo() {
        return deliveryNo;
    }

    public void setDeliveryNo(String deliveryNo) {
        this.deliveryNo = deliveryNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public String getLastEventName() {
        return lastEventName;
    }

    public void setLastEventName(String lastEventName) {
        this.lastEventName = lastEventName;
    }

    public String getLastLocationAltKey() {
        return lastLocationAltKey;
    }

    public void setLastLocationAltKey(String lastLocationAltKey) {
        this.lastLocationAltKey = lastLocationAltKey;
    }

    public String getLastVPLocationTypeCode() {
        return lastVPLocationTypeCode;
    }

    public void setLastVPLocationTypeCode(String lastVPLocationTypeCode) {
        this.lastVPLocationTypeCode = lastVPLocationTypeCode;
    }
}
