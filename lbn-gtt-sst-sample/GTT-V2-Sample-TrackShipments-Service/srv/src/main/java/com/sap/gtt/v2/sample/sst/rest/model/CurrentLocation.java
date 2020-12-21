package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class CurrentLocation {

    private UUID eventId;

    private String altKey;

    private String eventType;

    private String refPlannedEventType;

    private String refPlannedEventMatchKey;

    private String refPlannedEventLocationAltKey;

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String eventStatusCode;

    private List<EstimatedArrival> estimatedArrival;

    @JsonIgnore
    private PlannedEvent plannedEvent;

    public UUID getEventId() {
        return eventId;
    }

    public void setEventId(UUID eventId) {
        this.eventId = eventId;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
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

    public BigDecimal getLongitude() {
        return longitude;
    }

    public void setLongitude(BigDecimal longitude) {
        this.longitude = longitude;
    }

    public BigDecimal getLatitude() {
        return latitude;
    }

    public void setLatitude(BigDecimal latitude) {
        this.latitude = latitude;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public List<EstimatedArrival> getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(List<EstimatedArrival> estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
    }

    public PlannedEvent getPlannedEvent() {
        return plannedEvent;
    }

    public void setPlannedEvent(PlannedEvent plannedEvent) {
        this.plannedEvent = plannedEvent;
    }
}
