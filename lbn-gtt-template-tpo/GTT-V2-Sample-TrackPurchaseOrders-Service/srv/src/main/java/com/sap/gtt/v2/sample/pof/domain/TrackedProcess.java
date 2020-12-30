package com.sap.gtt.v2.sample.pof.domain;

import java.util.UUID;

public class TrackedProcess {
    private UUID id;
    private String trackedProcessType;
    private String altKey;
    private String trackingIdType;
    private String trackingId;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getTrackedProcessType() {
        return trackedProcessType;
    }

    public void setTrackedProcessType(String trackedProcessType) {
        this.trackedProcessType = trackedProcessType;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }
}
