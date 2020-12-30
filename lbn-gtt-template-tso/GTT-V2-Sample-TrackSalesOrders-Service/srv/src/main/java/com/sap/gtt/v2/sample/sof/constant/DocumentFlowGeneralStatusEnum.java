package com.sap.gtt.v2.sample.sof.constant;

/***
 * General status includes definition of node status, line status and group status.
 */
public enum DocumentFlowGeneralStatusEnum {
    INFORMATION("Information"),
    ERROR("Error");

    private final String status;

    DocumentFlowGeneralStatusEnum(String status) {
        this.status = status;
    }

    public String getStatus() {
        return status;
    }
}
