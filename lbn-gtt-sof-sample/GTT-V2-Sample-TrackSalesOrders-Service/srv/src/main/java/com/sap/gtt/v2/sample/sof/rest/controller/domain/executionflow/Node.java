package com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.List;
import java.util.UUID;

public class Node {

    private UUID eventId;
    private String eventType;
    private String eventStatus;
    private List<UUID> children;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String plannedAt;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String actualAt;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String eta;
    @JsonIgnore
    private Long plannedBusinessTimestamp;
    @JsonIgnore
    private Long actualBusinessTimestamp;
    @JsonIgnore
    private String locationAltKey;
    @JsonIgnore
    private String locationTypeCode;
    @JsonIgnore
    private String locationDescription;
    @JsonIgnore
    private Integer payloadSequence;

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

    public String getEventStatus() {
        return eventStatus;
    }

    public void setEventStatus(String eventStatus) {
        this.eventStatus = eventStatus;
    }

    public String getEta() {
        return eta;
    }

    public void setEta(String eta) {
        this.eta = eta;
    }

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public String getLocationDescription() {
        return locationDescription;
    }

    public void setLocationDescription(String locationDescription) {
        this.locationDescription = locationDescription;
    }

    public List<UUID> getChildren() {
        return children;
    }

    public void setChildren(List<UUID> children) {
        this.children = children;
    }

    public Long getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(Long plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public Long getActualBusinessTimestamp() {
        return actualBusinessTimestamp;
    }

    public void setActualBusinessTimestamp(Long actualBusinessTimestamp) {
        this.actualBusinessTimestamp = actualBusinessTimestamp;
    }

    public String getPlannedAt() {
        return plannedAt;
    }

    public void setPlannedAt(String plannedAt) {
        this.plannedAt = plannedAt;
    }

    public String getActualAt() {
        return actualAt;
    }

    public void setActualAt(String actualAt) {
        this.actualAt = actualAt;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public Integer getPayloadSequence() {
        return payloadSequence;
    }

    public void setPayloadSequence(Integer payloadSequence) {
        this.payloadSequence = payloadSequence;
    }

}
