package com.sap.gtt.v2.sample.sof.constant;

public enum DocumentFlowAttributeValueStatusEnum {
    VALUE_STATUS_INFORMATION(null),
    VALUE_STATUS_ERROR("ValueStatusError");

    private final String status;

    DocumentFlowAttributeValueStatusEnum(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }
}
