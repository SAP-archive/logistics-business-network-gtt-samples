package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

public class Event {
    private UUID id;
    private String altKey;
    private Long actualBusinessTimestamp;
    private String actualBusinessTimeZone;
    private Long actualTechnicalTimestamp;
    private String eventType;
    private BigDecimal longitude;
    private BigDecimal latitude;
    private Integer priority;
    private String locationAltKey;
    private String reportedBy;
    private String senderPartyId;
    private String partyId;
    private String eventMatchKey;
    private String trackingId;
    private String trackingIdType;
    private String eventReasonText;
    private String eventReasonCode;
    private Long creationDateTime;
    private List<ProcessEventDirectory> eventProcesses;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
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

    public Long getActualTechnicalTimestamp() {
        return actualTechnicalTimestamp;
    }

    public void setActualTechnicalTimestamp(Long actualTechnicalTimestamp) {
        this.actualTechnicalTimestamp = actualTechnicalTimestamp;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
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

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
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

    public String getPartyId() {
        return partyId;
    }

    public void setPartyId(String partyId) {
        this.partyId = partyId;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public String getEventReasonText() {
        return eventReasonText;
    }

    public void setEventReasonText(String eventReasonText) {
        this.eventReasonText = eventReasonText;
    }

    public String getEventReasonCode() {
        return eventReasonCode;
    }

    public void setEventReasonCode(String eventReasonCode) {
        this.eventReasonCode = eventReasonCode;
    }

    public Long getCreationDateTime() {
        return creationDateTime;
    }

    public void setCreationDateTime(Long creationDateTime) {
        this.creationDateTime = creationDateTime;
    }

    public List<ProcessEventDirectory> getEventProcesses() {
        return eventProcesses;
    }

    public void setEventProcesses(List<ProcessEventDirectory> eventProcesses) {
        this.eventProcesses = eventProcesses;
    }
}
