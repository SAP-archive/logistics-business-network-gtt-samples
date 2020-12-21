package com.sap.gtt.v2.sample.sst.common.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.gtt.v2.sample.sst.odata.model.LocationType;
import com.sap.gtt.v2.sample.sst.odata.model.TransportMeansStandardCode;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class Event {

    private UUID id;

    private String altKey;

    private Long actualBusinessTimestamp;

    private Long actualTechnicalTimestamp;

    private String actualBusinessTimeZone;

    private String eventType;

    private BigDecimal longitude;

    private BigDecimal latitude;

    private String locationAltKey;

    private String reportedBy;

    private String senderPartyId;

    private String eventMatchKey;

    private String trackingId;

    private Integer priority;

    private String eventReasonText;

    private String eventReasonCode;

    @JsonProperty("transportationMeans_code")
    private String transportationMeansCode;

    @JsonProperty("locationType_code")
    private String locationTypeCode;

    private String refPlannedEventType;

    private String refPlannedEventMatchKey;

    private String refPlannedEventLocationAltKey;

    private LocationType locationType;

    private TransportMeansStandardCode transportationMeans;

    private List<EstimatedArrival> estimatedArrival;

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

    public Long getActualTechnicalTimestamp() {
        return actualTechnicalTimestamp;
    }

    public void setActualTechnicalTimestamp(Long actualTechnicalTimestamp) {
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

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public Integer getPriority() {
        return priority;
    }

    public void setPriority(Integer priority) {
        this.priority = priority;
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

    public String getTransportationMeansCode() {
        return transportationMeansCode;
    }

    public void setTransportationMeansCode(String transportationMeansCode) {
        this.transportationMeansCode = transportationMeansCode;
    }

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
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

    public List<EstimatedArrival> getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(List<EstimatedArrival> estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
    }

    public LocationType getLocationType() {
        return locationType;
    }

    public void setLocationType(LocationType locationType) {
        this.locationType = locationType;
    }

    public TransportMeansStandardCode getTransportationMeans() {
        return transportationMeans;
    }

    public void setTransportationMeans(TransportMeansStandardCode transportationMeans) {
        this.transportationMeans = transportationMeans;
    }
}
