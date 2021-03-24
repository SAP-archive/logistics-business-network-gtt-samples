package com.sap.gtt.v2.sample.pof.domain;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;
import java.util.UUID;

public class PlannedEvent implements Serializable {
    private Long plannedBusinessTimestamp;
    private String plannedBusinessTimeZone;
    private Long plannedBizTsEarliest;
    private Long plannedBizTsLatest;
    private String eventType;
    private UUID id;
    @SerializedName("process_id")
    private UUID processId;
    @SerializedName("eventStatus_code")
    private String eventStatusCode;
    @SerializedName("lastProcessEventDirectory_id")
    private UUID lastProcessEventDirectoryId;
    private String locationAltKey;
    private String eventMatchKey;
    private BigDecimal longitude;
    private BigDecimal latitude;
    private LocationDTO locationDTO;
    private Integer payloadSequence;
    private ProcessEventDirectory lastProcessEventDirectory;

    public LocationDTO getLocationDTO() {
        return locationDTO;
    }

    public void setLocationDTO(LocationDTO locationDTO) {
        this.locationDTO = locationDTO;
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

    public Long getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(Long plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }

    public String getPlannedBusinessTimeZone() {
        return plannedBusinessTimeZone;
    }

    public void setPlannedBusinessTimeZone(String plannedBusinessTimeZone) {
        this.plannedBusinessTimeZone = plannedBusinessTimeZone;
    }

    public String getEventType() {
        return eventType;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }

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

    public ProcessEventDirectory getLastProcessEventDirectory() {
        return lastProcessEventDirectory;
    }

    public void setLastProcessEventDirectory(ProcessEventDirectory lastProcessEventDirectory) {
        this.lastProcessEventDirectory = lastProcessEventDirectory;
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

    public Integer getPayloadSequence() {
        return payloadSequence;
    }

    public void setPayloadSequence(Integer payloadSequence) {
        this.payloadSequence = payloadSequence;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PlannedEvent that = (PlannedEvent) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

}
