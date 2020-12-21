package com.sap.gtt.v2.sample.sst.rest.model;

import java.math.BigDecimal;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class EventHistory {

    private UUID id;

    private String altKey;

    private String actualBusinessTimestamp;

    private String actualTechnicalTimestamp;

    private String actualBusinessTimeZone;

    private String eventType;

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String locationAltKey;

    private String reportedBy;

    private String senderPartyId;

    private String eventMatchKey;

    private String locationType;

    private String eta;

    private String refPlannedEventType;

    private String refPlannedEventMatchKey;

    private String refPlannedEventLocationAltKey;

    private String eventProcesses;

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

    public String getActualBusinessTimestamp() {
        return actualBusinessTimestamp;
    }

    public void setActualBusinessTimestamp(String actualBusinessTimestamp) {
        this.actualBusinessTimestamp = actualBusinessTimestamp;
    }

    public String getActualTechnicalTimestamp() {
        return actualTechnicalTimestamp;
    }

    public void setActualTechnicalTimestamp(String actualTechnicalTimestamp) {
        this.actualTechnicalTimestamp = actualTechnicalTimestamp;
    }

    public String getActualBusinessTimeZone() {
        return actualBusinessTimeZone;
    }

    public void setActualBusinessTimeZone(String actualBusinessTimeZone) {
        this.actualBusinessTimeZone = actualBusinessTimeZone;
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

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    public String getLocationType() {
        return locationType;
    }

    public void setLocationType(String locationType) {
        this.locationType = locationType;
    }

    public String getEta() {
        return eta;
    }

    public void setEta(String eta) {
        this.eta = eta;
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

    public String getEventProcesses() {
        return eventProcesses;
    }

    public void setEventProcesses(String eventProcesses) {
        this.eventProcesses = eventProcesses;
    }
}
