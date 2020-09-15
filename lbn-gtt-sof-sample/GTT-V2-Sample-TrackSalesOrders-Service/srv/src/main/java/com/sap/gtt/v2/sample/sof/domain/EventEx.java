package com.sap.gtt.v2.sample.sof.domain;


import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.gson.annotations.SerializedName;

import java.util.List;

public class EventEx extends Event {

    @SerializedName("locationType_code")
    private String locationTypeCode;
    private String actualAt;
    private String refPlannedEventType;
    private String refPlannedEventMatchKey;
    private String refPlannedEventLocationAltKey;

    @JsonIgnore
    private List<EstimatedArrival> estimatedArrival;

    private List<ProcessEventDirectory> eventProcesses;

    public List<ProcessEventDirectory> getEventProcesses() {
        return eventProcesses;
    }

    public void setEventProcesses(List<ProcessEventDirectory> eventProcesses) {
        this.eventProcesses = eventProcesses;
    }


    public String getRefPlannedEventType() {
        return refPlannedEventType;
    }

    public void setRefPlannedEventType(String refPlannedEventType) {
        this.refPlannedEventType = refPlannedEventType;
    }

    public String getRefPlannedEventMatchKey() {
        return refPlannedEventMatchKey;
    }

    public void setRefPlannedEventMatchKey(String refPlannedEventMatchKey) {
        this.refPlannedEventMatchKey = refPlannedEventMatchKey;
    }

    public String getRefPlannedEventLocationAltKey() {
        return refPlannedEventLocationAltKey;
    }

    public void setRefPlannedEventLocationAltKey(String refPlannedEventLocationAltKey) {
        this.refPlannedEventLocationAltKey = refPlannedEventLocationAltKey;
    }

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public List<EstimatedArrival> getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(List<EstimatedArrival> estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
    }

    public String getActualAt() {
        return actualAt;
    }

    public void setActualAt(String actualAt) {
        this.actualAt = actualAt;
    }
}
