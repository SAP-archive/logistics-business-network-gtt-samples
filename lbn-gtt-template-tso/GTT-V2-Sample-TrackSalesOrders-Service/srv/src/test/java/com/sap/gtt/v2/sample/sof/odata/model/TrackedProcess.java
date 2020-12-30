package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.MODEL_NAMESPACE;

@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = "TrackedProcess", container = Constants.ENTITY_CONTAINER_NAME)
public class TrackedProcess {
    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "subaccountId")
    private UUID subaccountId;

    @EdmProperty(name = "cloneInstanceId")
    private UUID cloneInstanceId;

    @EdmProperty(name = "altKey")
    private String altKey;

    @EdmProperty(name = "trackingId")
    private String trackingId;

    @EdmProperty(name = "lastChangedAtBusinessTime", type = EdmType.DATE_TIME_OFFSET)
    private Long lastChangedAtBusinessTime;

    @EdmProperty(name = "creationDateTime", type = EdmType.DATE_TIME_OFFSET)
    private Long creationDateTime;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "plannedEvents", toType = PlannedEvent.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<PlannedEvent> plannedEvents = new ArrayList<>();


    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public UUID getSubaccountId() {
        return subaccountId;
    }

    public void setSubaccountId(UUID subaccountId) {
        this.subaccountId = subaccountId;
    }

    public UUID getCloneInstanceId() {
        return cloneInstanceId;
    }

    public void setCloneInstanceId(UUID cloneInstanceId) {
        this.cloneInstanceId = cloneInstanceId;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public Long getLastChangedAtBusinessTime() {
        return lastChangedAtBusinessTime;
    }

    public void setLastChangedAtBusinessTime(Long lastChangedAtBusinessTime) {
        this.lastChangedAtBusinessTime = lastChangedAtBusinessTime;
    }

    public Long getCreationDateTime() {
        return creationDateTime;
    }

    public void setCreationDateTime(Long creationDateTime) {
        this.creationDateTime = creationDateTime;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus =processStatus;
    }

    public List<PlannedEvent> getPlannedEvents() {
        return plannedEvents;
    }

    public void setPlannedEvents(List<PlannedEvent> plannedEvents) {
        this.plannedEvents = plannedEvents;
    }

    @Override
    public String toString() {
        return "TrackedProcess{" +
                "id=" + id +
                ", subaccountId=" + subaccountId +
                ", cloneInstanceId=" + cloneInstanceId +
                ", altKey='" + altKey + '\'' +
                ", trackingId='" + trackingId + '\'' +
                ", lastChangedAtBusinessTime=" + lastChangedAtBusinessTime +
                ", creationDateTime=" + creationDateTime +
                ", processStatus=" + processStatus +
                ", plannedEvents=" + plannedEvents +
                '}';
    }
}
