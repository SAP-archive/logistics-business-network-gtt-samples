package com.sap.gtt.v2.sample.sof.domain;

import java.math.BigDecimal;
import java.util.UUID;

public class Event {
    private UUID id;
    private String altKey;
    private Long actualBusinessTimestamp;
    private String actualBusinessTimeZone;
    private String eventType;
    private BigDecimal longitude;
    private BigDecimal latitude;
    private String locationAltKey;
    private String reportedBy;
    private String senderPartyId;
    private String eventMatchKey;
    private String trackingId;
    private String eventReasonText;

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


    public BigDecimal getLongitude() {
        return longitude;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
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

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public Long getActualBusinessTimestamp() {
        return actualBusinessTimestamp;
    }

    public void setActualBusinessTimestamp(Long actualBusinessTimestamp) {
        this.actualBusinessTimestamp = actualBusinessTimestamp;
    }

    public String getActualBusinessTimeZone() {
        return actualBusinessTimeZone;
    }

    public void setActualBusinessTimeZone(String actualBusinessTimeZone) {
        this.actualBusinessTimeZone = actualBusinessTimeZone;
    }

    public String getReportedBy() {
        return reportedBy;
    }

    public void setReportedBy(String reportedBy) {
        this.reportedBy = reportedBy;
    }

    public String getSenderPartyId() {
        return senderPartyId;
    }

    public void setSenderPartyId(String senderPartyId) {
        this.senderPartyId = senderPartyId;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public String getEventReasonText() {
        return eventReasonText;
    }

    public void setEventReasonText(String eventReasonText) {
        this.eventReasonText = eventReasonText;
    }
}
