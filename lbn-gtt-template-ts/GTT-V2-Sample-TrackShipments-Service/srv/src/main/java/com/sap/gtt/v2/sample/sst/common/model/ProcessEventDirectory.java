package com.sap.gtt.v2.sample.sst.common.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.util.UUID;

/**
 * @author Aliaksandr Miron
 */
public class ProcessEventDirectory {

    private UUID id;

    @SerializedName("process_id")
    private UUID processId;

    @SerializedName("plannedEvent_id")
    private UUID plannedEventId;

    @SerializedName("event_id")
    private UUID eventId;

    @SerializedName("correlationType_code")
    private String correlationTypeCode;

    private Event event;

    private PlannedEvent plannedEvent;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public UUID getProcessId() {
        return processId;
    }

    public void setProcessId(UUID processId) {
        this.processId = processId;
    }

    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }

    public UUID getEventId() {
        return eventId;
    }

    public void setEventId(UUID eventId) {
        this.eventId = eventId;
    }

    public String getCorrelationTypeCode() {
        return correlationTypeCode;
    }

    public void setCorrelationTypeCode(String correlationTypeCode) {
        this.correlationTypeCode = correlationTypeCode;
    }

    public Event getEvent() {
        return event;
    }

    public void setEvent(Event event) {
        this.event = event;
    }

    public PlannedEvent getPlannedEvent() {
        return plannedEvent;
    }

    public void setPlannedEvent(PlannedEvent plannedEvent) {
        this.plannedEvent = plannedEvent;
    }
}
