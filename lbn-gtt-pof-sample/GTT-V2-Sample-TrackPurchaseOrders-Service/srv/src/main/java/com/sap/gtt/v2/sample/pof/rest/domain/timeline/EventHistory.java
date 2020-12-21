package com.sap.gtt.v2.sample.pof.rest.domain.timeline;

import com.sap.gtt.v2.sample.pof.utils.POFUtils;

import java.math.BigDecimal;
import java.util.UUID;

import static java.util.Objects.nonNull;

public class EventHistory {
    private UUID id;

    private String altKey;

    private String actualBusinessTimestamp;

    private String actualBusinessTimeZone;

    private String eventType;

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String locationAltKey;

    private String reportedBy;

    private String senderPartyId;

    private String eventMatchKey;

    private Long actualTechTimestamp;

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

    public void setActualBusinessTimestamp(Long actualBusinessTimestamp) {
        if (nonNull(actualBusinessTimestamp)) {
            this.actualBusinessTimestamp = POFUtils.getUTCTimeString(actualBusinessTimestamp);
        } else {
            this.actualBusinessTimestamp = null;
        }
    }

    public String getActualBusinessTimeZone() {
        return actualBusinessTimeZone;
    }

    public void setActualBusinessTimeZone(String actualBusinessTimeZone) {
        this.actualBusinessTimeZone = actualBusinessTimeZone;
    }

    public Long getActualTechTimestamp() {
        return actualTechTimestamp;
    }

    public void setActualTechTimestamp(Long actualTechTimestamp) {
        this.actualTechTimestamp = actualTechTimestamp;
    }

    public String getEventType() {
        this.eventType = eventType.substring(eventType.lastIndexOf('.') + 1);
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

}
