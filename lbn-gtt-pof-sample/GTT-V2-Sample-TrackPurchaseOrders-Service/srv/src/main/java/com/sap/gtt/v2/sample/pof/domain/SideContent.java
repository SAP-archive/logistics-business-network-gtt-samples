package com.sap.gtt.v2.sample.pof.domain;


import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;

import java.util.UUID;

public class SideContent {

    private String eventType;
    private String eventStatusCode;
    private String plannedBusinessTimestamp;
    private String locationAltKey;
    private LocationDTO location;
    private String actualBusinessTimestamp;
    private UUID plannedEventId;
    private boolean isPlannedEvent;
    private boolean isActualEvent;
    private String eventMatchKey;
    private String eventTypeFullName;
    private String eventReasonText;

    public String getEventReasonText() {
        return eventReasonText;
    }

    public void setEventReasonText(String eventReasonText) {
        this.eventReasonText = eventReasonText;
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

    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }
    public boolean getIsActualEvent() {
        return isActualEvent;
    }
    public void setIsActualEvent(boolean isActualEvent) {
        this.isActualEvent = isActualEvent;
    }
    public boolean getIsPlannedEvent() {
        return isPlannedEvent;
    }

    public void setIsPlannedEvent(boolean isPlannedEvent) {
        this.isPlannedEvent = isPlannedEvent;
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


    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public LocationDTO getLocation() {
        return location;
    }

    public void setLocation(LocationDTO location) {
        this.location = location;
    }

    public String getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(String plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public String getActualBusinessTimestamp() {
        return actualBusinessTimestamp;
    }

    public void setActualBusinessTimestamp(String actualBusinessTimestamp) {
        this.actualBusinessTimestamp = actualBusinessTimestamp;
    }

}
