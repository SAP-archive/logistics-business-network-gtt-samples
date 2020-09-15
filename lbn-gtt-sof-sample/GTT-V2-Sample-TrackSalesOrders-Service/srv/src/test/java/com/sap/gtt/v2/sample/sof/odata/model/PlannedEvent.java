package com.sap.gtt.v2.sample.sof.odata.model;


import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;
import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.MODEL_NAMESPACE;

@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = "PlannedEvent", container = Constants.ENTITY_CONTAINER_NAME)
public class PlannedEvent {

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "process_id")
    private UUID processId;

    @EdmProperty(name = "longitude")
    private BigDecimal longitude;

    @EdmProperty(name = "latitude")
    private BigDecimal latitude;

    @EdmNavigationProperty(name = "eventStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus eventStatus;

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

    public ProcessStatus getEventStatus() {
        return eventStatus;
    }

    public void setEventStatus(ProcessStatus eventStatus) {
        this.eventStatus = eventStatus;
    }

    @Override
    public String toString() {
        return "PlannedEvent{" +
                "id=" + id +
                ", processId=" + processId +
                ", longitude=" + longitude +
                ", latitude=" + latitude +
                ", eventStatus=" + eventStatus +
                '}';
    }
}
