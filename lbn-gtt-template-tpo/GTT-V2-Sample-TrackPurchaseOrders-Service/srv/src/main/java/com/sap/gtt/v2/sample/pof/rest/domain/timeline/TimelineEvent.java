package com.sap.gtt.v2.sample.pof.rest.domain.timeline;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;

import java.util.List;
import java.util.UUID;

import static java.util.Objects.nonNull;

public class TimelineEvent {

    private String eventType;

    private String eventStatusCode;

    private String plannedBusinessTimestamp;

    private String locationAltKey;

    private String locationId;

    private Location location;

    private String actualBusinessTimestamp;

    private UUID plannedEventId;

    private UUID actualEventId;

    private String eventMatchKey;

    private String eventTypeFullName;

    @JsonProperty("historicalEvents")
    private List<EventHistory> eventHistory;

    public String getEventType() {
        this.eventType = eventTypeFullName.substring(eventTypeFullName.lastIndexOf('.') + 1);
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

    public void setPlannedBusinessTimestamp(Long plannedBusinessTimestamp) {
        if (nonNull(plannedBusinessTimestamp)) {
            this.plannedBusinessTimestamp = POFUtils.getUTCTimeString(plannedBusinessTimestamp);
        } else {
            this.plannedBusinessTimestamp = null;
        }
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public String getActualBusinessTimestamp() {
        return actualBusinessTimestamp;
    }

    public void setActualBusinessTimestamp(Long actualBusinessTimestamp) {
        if (nonNull(actualBusinessTimestamp)) {
            this.actualBusinessTimestamp = POFUtils.getUTCTimeString(actualBusinessTimestamp);
        }else {
            this.actualBusinessTimestamp = null;
        }
    }
    public void setActualBusinessTimestamp(String actualBusinessTimestamp) {
        this.actualBusinessTimestamp = actualBusinessTimestamp;
    }
    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }

    public UUID getActualEventId() {
        return actualEventId;
    }

    public void setActualEventId(UUID actualEventId) {
        this.actualEventId = actualEventId;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    public String getEventTypeFullName() {
        return eventTypeFullName;
    }

    public void setEventTypeFullName(String eventTypeFullName) {
        this.eventTypeFullName = eventTypeFullName;
    }

    public List<EventHistory> getEventHistory() {
        return eventHistory;
    }

    public void setEventHistory(List<EventHistory> eventHistory) {
        this.eventHistory = eventHistory;
    }

    public void setPlannedBusinessTimestamp(String plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public String getLocationId() {
        return locationId;
    }

    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }
}
