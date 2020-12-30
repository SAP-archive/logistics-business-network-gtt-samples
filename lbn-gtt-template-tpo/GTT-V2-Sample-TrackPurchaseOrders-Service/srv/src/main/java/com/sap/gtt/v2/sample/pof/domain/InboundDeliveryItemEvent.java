package com.sap.gtt.v2.sample.pof.domain;

import com.google.gson.annotations.SerializedName;

public class InboundDeliveryItemEvent extends EventForWrite {
    private String inboundDeliveryNo;
    private String itemNo;
    @SerializedName("executionStatus_code")
    private String executionStatusCode;

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
}
