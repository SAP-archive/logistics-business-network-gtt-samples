package com.sap.gtt.v2.sample.pof.domain;

import com.google.gson.annotations.SerializedName;

public class InboundDeliveryItemEvent extends EventForWrite {
    private String inboundDeliveryNo;
    private String itemNo;
    @SerializedName("executionStatus_code")
    private String executionStatusCode;

    private String lastEventName;

    private String lastLocationAltKey;

    @SerializedName("lastVPLocationType_code")
    private String lastVPLocationTypeCode;

    public String getInboundDeliveryNo() {
        return inboundDeliveryNo;
    }

    public void setInboundDeliveryNo(String inboundDeliveryNo) {
        this.inboundDeliveryNo = inboundDeliveryNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
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
