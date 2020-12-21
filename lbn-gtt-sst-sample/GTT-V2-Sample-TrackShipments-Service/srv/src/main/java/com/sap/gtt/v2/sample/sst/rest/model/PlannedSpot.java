package com.sap.gtt.v2.sample.sst.rest.model;

import java.math.BigDecimal;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class PlannedSpot {

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String eventMatchKey;

    private String locationAltKey;

    private UUID eventId;

    private String eventType;

    private String eventStatusCode;

    private String plannedBusinessTimestamp;

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

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public UUID getEventId() {
        return eventId;
    }

    public void setEventId(UUID eventId) {
        this.eventId = eventId;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public String getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(String plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

}
