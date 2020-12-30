package com.sap.gtt.v2.sample.pof.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

public class PlannedSpot implements Serializable {

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String eventMatchKey;

    private String objectTypeCode;

    private UUID eventId;

    private String locationDescription;

    @JsonIgnore
    private String eventTypeOrign;

    private String eventType;

    private UUID plannedEventId;

    private EstimatedArrival estimatedArrival;

    private String eventStatusCode;

    private String plannedBusinessTimestamp;

    private String locationAltKey;

    public String getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(String plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }
    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }

    public String getEventTypeOrign() {
        return eventTypeOrign;
    }

    public void setEventTypeOrign(String eventTypeOrign) {
        this.eventTypeOrign = eventTypeOrign;
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

    public String getLocationDescription() {
        return locationDescription;
    }

    public void setLocationDescription(String locationDescription) {
        this.locationDescription = locationDescription;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    public UUID getEventId() {
        return eventId;
    }

    public void setEventId(UUID eventId) {
        this.eventId = eventId;
    }

    public String getObjectTypeCode() {
        return objectTypeCode;
    }

    public void setObjectTypeCode(String objectTypeCode) {
        this.objectTypeCode = objectTypeCode;
    }



    public PlannedSpot(UUID eventId, BigDecimal longitude, BigDecimal latitude,  String objectTypeCode) {
        this.eventId = eventId;
        this.longitude = longitude;
        this.latitude = latitude;
        this.objectTypeCode = objectTypeCode;
    }

    public PlannedSpot() {
    }

    public BigDecimal getLongitude() {
        return longitude;
    }

    public BigDecimal getLatitude() {
        return latitude;
    }

    public void setLongitude(BigDecimal longitude) {
        this.longitude = longitude;
    }

    public void setLatitude(BigDecimal latitude) {
        this.latitude = latitude;
    }

    public EstimatedArrival getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(EstimatedArrival estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
    }
}
