package com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow;

import java.util.UUID;

public class TPRelation {

    private UUID fromTrackedProcessUUID;
    private UUID toTrackedProcessUUID;
    private String toTrackedProcessStatus;

    public TPRelation(UUID fromTrackedProcessUUID, UUID toTrackedProcessUUID, String tpRelations) {
        this.fromTrackedProcessUUID = fromTrackedProcessUUID;
        this.toTrackedProcessUUID = toTrackedProcessUUID;
        this.toTrackedProcessStatus = tpRelations;
    }

    public UUID getFromTrackedProcessUUID() {
        return fromTrackedProcessUUID;
    }

    public void setFromTrackedProcessUUID(UUID fromTrackedProcessUUID) {
        this.fromTrackedProcessUUID = fromTrackedProcessUUID;
    }

    public UUID getToTrackedProcessUUID() {
        return toTrackedProcessUUID;
    }

    public void setToTrackedProcessUUID(UUID toTrackedProcessUUID) {
        this.toTrackedProcessUUID = toTrackedProcessUUID;
    }

    public String getToTrackedProcessStatus() {
        return toTrackedProcessStatus;
    }

    public void setToTrackedProcessStatus(String toTrackedProcessStatus) {
        this.toTrackedProcessStatus = toTrackedProcessStatus;
    }
}
