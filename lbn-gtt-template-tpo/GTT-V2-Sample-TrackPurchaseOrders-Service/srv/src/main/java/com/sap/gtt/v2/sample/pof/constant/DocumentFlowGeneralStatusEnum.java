package com.sap.gtt.v2.sample.pof.constant;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;

import java.util.Arrays;

/***
 * General status includes definition of node status, line status and group status.
 */
public enum DocumentFlowGeneralStatusEnum {
    INFORMATION("Information", 1),
    SUCCESS("Success", 2),
    WARNING("Warning", 3),
    ERROR("Error", 4);

    private final String status;
    private final int weight;

    DocumentFlowGeneralStatusEnum(String status, int weight) {
        this.status = status;
        this.weight = weight;
    }

    public String getStatus() {
        return status;
    }

    public static DocumentFlowGeneralStatusEnum getByStatus(String status) {
        return Arrays.stream(values())
                .filter(v -> v.getStatus().equals(status))
                .findFirst()
                .orElseThrow(() -> new POFServiceException(String.format("Unsupportable DocumentFlowGeneralStatusEnum %s", status)));
    }

    public static int compareByStatuses(String left, String right) {
        return getByStatus(left).weight - getByStatus(right).weight;
    }

    public int getWeight() {
        return weight;
    }
}
