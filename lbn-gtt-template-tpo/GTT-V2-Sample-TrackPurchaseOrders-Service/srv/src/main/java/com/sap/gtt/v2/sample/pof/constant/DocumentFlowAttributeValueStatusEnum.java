package com.sap.gtt.v2.sample.pof.constant;

public enum DocumentFlowAttributeValueStatusEnum {
    INFORMATION("ValueStatusInformation"),
    SUCCESS("ValueStatusSuccess"),
    ERROR("ValueStatusError"),
    WARNING("ValueStatusWarning");

    private final String status;

    DocumentFlowAttributeValueStatusEnum(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }
}
