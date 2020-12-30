package com.sap.gtt.v2.sample.pof.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

public class CurrentLocation implements Serializable {
    private UUID eventId;
    private String altKey;
    @JsonIgnore
    private String eventTypeOrign;
    private String refPlannedEventType;
    private String refPlannedEventMatchKey;
    private String refPlannedEventLocationAltKey;
    private BigDecimal longitude;
    private BigDecimal latitude;
    @JsonIgnore
    private List<EstimatedArrival> estimatedArrival;
    private String eventType;
    private UUID plannedEventId;

    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }

    public String getEventType() {
        if(StringUtils.isNotBlank(eventTypeOrign)) {
            return eventTypeOrign.substring(eventTypeOrign.lastIndexOf(".") + 1);
        }
        return eventTypeOrign;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public List<EstimatedArrival> getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(List<EstimatedArrival> estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
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

    public String getEventTypeOrign() {
        return eventTypeOrign;
    }

    public void setEventTypeOrign(String eventTypeOrign) {
        this.eventTypeOrign = eventTypeOrign;
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
}
