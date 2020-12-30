package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.LocationType;
import java.util.List;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class TimelineEvent {

    private String eventType;

    private String eventStatusCode;

    private String plannedBusinessTimestamp;

    private String locationAltKey;

    @JsonIgnore
    private String actualEventLocationAltKey;

    @JsonIgnore
    private String plannedEventLocationAltKey;

    private Location location;

    private LocationType locationType;

    private String actualBusinessTimestamp;

    private String actualTechnicalTimestamp;

    private UUID plannedEventId;

    private UUID actualEventId;

    private String eventMatchKey;

    private String eventTypeFullName;

    private String eventReasonText;

    @JsonProperty("historicalEvents")
    private List<EventHistory> eventHistory;

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

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public String getActualEventLocationAltKey() {
        return actualEventLocationAltKey;
    }

    public void setActualEventLocationAltKey(String actualEventLocationAltKey) {
        this.actualEventLocationAltKey = actualEventLocationAltKey;
    }

    public String getPlannedEventLocationAltKey() {
        return plannedEventLocationAltKey;
    }

    public void setPlannedEventLocationAltKey(String plannedEventLocationAltKey) {
        this.plannedEventLocationAltKey = plannedEventLocationAltKey;
    }

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public LocationType getLocationType() {
        return locationType;
    }

    public void setLocationType(LocationType locationType) {
        this.locationType = locationType;
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

    public String getEventReasonText() {
        return eventReasonText;
    }

    public void setEventReasonText(String eventReasonText) {
        this.eventReasonText = eventReasonText;
    }

    public List<EventHistory> getEventHistory() {
        return eventHistory;
    }

    public void setEventHistory(List<EventHistory> eventHistory) {
        this.eventHistory = eventHistory;
    }
}
