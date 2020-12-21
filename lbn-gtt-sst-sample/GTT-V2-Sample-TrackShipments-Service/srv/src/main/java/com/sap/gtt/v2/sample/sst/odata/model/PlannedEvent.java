package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmType.DATE_TIME_OFFSET;

import com.google.gson.annotations.SerializedName;
import java.math.BigDecimal;
import java.util.Objects;
import java.util.UUID;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = PlannedEvent.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class PlannedEvent {

    public static final String ENTITY_SET_NAME = "PlannedEvent";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "eventType")
    private String eventType;

    @EdmProperty(name = "locationAltKey")
    private String locationAltKey;

    @EdmProperty(name = "eventMatchKey")
    private String eventMatchKey;

    @EdmProperty(name = "longitude")
    private BigDecimal longitude;

    @EdmProperty(name = "latitude")
    private BigDecimal latitude;

    @EdmProperty(name = "isFinalPlannedEvent")
    private Boolean isFinalPlannedEvent;

    @EdmProperty(name = "plannedTechnicalTimestamp", type = DATE_TIME_OFFSET)
    private Long plannedTechnicalTimestamp;

    @EdmProperty(name = "plannedTechTsEarliest", type = DATE_TIME_OFFSET)
    private Long plannedTechTsEarliest;

    @EdmProperty(name = "plannedTechTsLatest", type = DATE_TIME_OFFSET)
    private Long plannedTechTsLatest;

    @EdmProperty(name = "plannedBusinessTimestamp", type = DATE_TIME_OFFSET)
    private Long plannedBusinessTimestamp;

    @EdmProperty(name = "plannedBizTsEarliest", type = DATE_TIME_OFFSET)
    private Long plannedBizTsEarliest;

    @EdmProperty(name = "plannedBizTsLatest", type = DATE_TIME_OFFSET)
    private Long plannedBizTsLatest;

    @EdmProperty(name = "plannedBusinessTimeZone")
    private String plannedBusinessTimeZone;

    @SerializedName("process_id")
    @EdmProperty(name = "process_id")
    private UUID processId;

    @EdmProperty(name = "payloadSequence")
    private Integer payloadSequence;

    @EdmProperty(name = "nextOverdueDetection", type = DATE_TIME_OFFSET)
    private Long nextOverdueDetection;

    @EdmProperty(name = "overdueDetectionCounter")
    private Integer overdueDetectionCounter;

    @SerializedName("eventStatus_code")
    @EdmProperty(name = "eventStatus_code")
    private String eventStatusCode;

    @SerializedName("lastProcessEventDirectory_id")
    @EdmProperty(name = "lastProcessEventDirectory_id")
    private UUID lastProcessEventDirectoryId;

    @EdmNavigationProperty(name = "eventStatus", toType = ProcessStatus.class, toMultiplicity = ONE)
    private ProcessStatus eventStatus;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
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

    public Boolean getFinalPlannedEvent() {
        return isFinalPlannedEvent;
    }

    public void setFinalPlannedEvent(Boolean finalPlannedEvent) {
        isFinalPlannedEvent = finalPlannedEvent;
    }

    public Long getPlannedTechnicalTimestamp() {
        return plannedTechnicalTimestamp;
    }

    public void setPlannedTechnicalTimestamp(Long plannedTechnicalTimestamp) {
        this.plannedTechnicalTimestamp = plannedTechnicalTimestamp;
    }

    public Long getPlannedTechTsEarliest() {
        return plannedTechTsEarliest;
    }

    public void setPlannedTechTsEarliest(Long plannedTechTsEarliest) {
        this.plannedTechTsEarliest = plannedTechTsEarliest;
    }

    public Long getPlannedTechTsLatest() {
        return plannedTechTsLatest;
    }

    public void setPlannedTechTsLatest(Long plannedTechTsLatest) {
        this.plannedTechTsLatest = plannedTechTsLatest;
    }

    public Long getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(Long plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public Long getPlannedBizTsEarliest() {
        return plannedBizTsEarliest;
    }

    public void setPlannedBizTsEarliest(Long plannedBizTsEarliest) {
        this.plannedBizTsEarliest = plannedBizTsEarliest;
    }

    public Long getPlannedBizTsLatest() {
        return plannedBizTsLatest;
    }

    public void setPlannedBizTsLatest(Long plannedBizTsLatest) {
        this.plannedBizTsLatest = plannedBizTsLatest;
    }

    public String getPlannedBusinessTimeZone() {
        return plannedBusinessTimeZone;
    }

    public void setPlannedBusinessTimeZone(String plannedBusinessTimeZone) {
        this.plannedBusinessTimeZone = plannedBusinessTimeZone;
    }

    public UUID getProcessId() {
        return processId;
    }

    public void setProcessId(UUID processId) {
        this.processId = processId;
    }

    public Integer getPayloadSequence() {
        return payloadSequence;
    }

    public void setPayloadSequence(Integer payloadSequence) {
        this.payloadSequence = payloadSequence;
    }

    public Long getNextOverdueDetection() {
        return nextOverdueDetection;
    }

    public void setNextOverdueDetection(Long nextOverdueDetection) {
        this.nextOverdueDetection = nextOverdueDetection;
    }

    public Integer getOverdueDetectionCounter() {
        return overdueDetectionCounter;
    }

    public void setOverdueDetectionCounter(Integer overdueDetectionCounter) {
        this.overdueDetectionCounter = overdueDetectionCounter;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public UUID getLastProcessEventDirectoryId() {
        return lastProcessEventDirectoryId;
    }

    public void setLastProcessEventDirectoryId(UUID lastProcessEventDirectoryId) {
        this.lastProcessEventDirectoryId = lastProcessEventDirectoryId;
    }

    public ProcessStatus getEventStatus() {
        return eventStatus;
    }

    public void setEventStatus(ProcessStatus eventStatus) {
        this.eventStatus = eventStatus;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        PlannedEvent that = (PlannedEvent) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
