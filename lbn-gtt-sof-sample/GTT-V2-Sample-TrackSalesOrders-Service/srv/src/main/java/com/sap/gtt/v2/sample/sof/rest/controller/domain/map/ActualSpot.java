package com.sap.gtt.v2.sample.sof.rest.controller.domain.map;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.sap.gtt.v2.sample.sof.odata.model.LocationDTO;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.UUID;

public class ActualSpot implements Serializable {

    private BigDecimal longitude;
    private BigDecimal latitude;
    private String altKey;
    private UUID eventId;
    @JsonIgnore
    private String eventTypeOrign;
    private String eventType;
    private UUID plannedEventId;
    private String eventMatchKey;
    private String locationAltKey;
    private LocationDTO location = new LocationDTO();
    private String objectTypeCode;
    private String locationDescription;
    private String locationTypeCode;

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public String getObjectTypeCode() {
        return objectTypeCode;
    }

    public void setObjectTypeCode(String objectTypeCode) {
        this.objectTypeCode = objectTypeCode;
    }

    public String getLocationDescription() {
        return locationDescription;
    }

    public void setLocationDescription(String locationDescription) {
        this.locationDescription = locationDescription;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
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

    public UUID getPlannedEventId() {
        return plannedEventId;
    }

    public void setPlannedEventId(UUID plannedEventId) {
        this.plannedEventId = plannedEventId;
    }

    public String getEventType() {
        if(StringUtils.isNotBlank(eventTypeOrign)) {
            return eventTypeOrign.substring(eventTypeOrign.lastIndexOf(".") + 1);
        }
        return eventTypeOrign;
    }

    public void setEventType(String eventType) {
        this.eventType = eventType;
    }
    @JsonIgnore
    public String getEventTypeOrign() {
        return eventTypeOrign;
    }

    public void setEventTypeOrign(String eventTypeOrign) {
        this.eventTypeOrign = eventTypeOrign;
    }

    public UUID getEventId() {
        return eventId;
    }

    public void setEventId(UUID eventId) {
        this.eventId = eventId;
    }


    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public ActualSpot() {
    }

    public BigDecimal getLongitude() {
        return longitude;
    }

    public BigDecimal getLatitude() {
        return latitude;
    }

    public void setLongitude(BigDecimal longitude) {
        this.longitude = longitude;
    }

    public void setLatitude(BigDecimal latitude) {
        this.latitude = latitude;
    }
}
