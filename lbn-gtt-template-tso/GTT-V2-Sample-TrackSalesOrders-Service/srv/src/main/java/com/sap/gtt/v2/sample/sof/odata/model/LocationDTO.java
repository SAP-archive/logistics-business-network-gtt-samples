package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

import java.io.Serializable;
import java.math.BigDecimal;
@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = LocationDTO.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class LocationDTO implements Serializable {
    public static final String ENTITY_SET_NAME = "LocationDTO";
    @EdmProperty(name = "locationDescription")
    private String locationDescription;
    @EdmKey
    @EdmProperty(name = "locationAltKey")
    private String locationAltKey;
    @EdmProperty(name = "sourceSystem")
    private String sourceSystem;
    @EdmProperty(name = "externalId")
    private String externalId;
    @EdmProperty(name = "objectTypeDescription")
    private String objectTypeDescription;
    @EdmProperty(name = "formattedAddress")
    private String formattedAddress;
    @EdmProperty(name = "locationId")
    private String locationId;
    @EdmProperty(name = "longitude")
    private BigDecimal longitude;
    @EdmProperty(name = "latitude")
    private BigDecimal latitude;
    @EdmProperty(name = "objectTypeCode")
    private String objectTypeCode;
    @EdmProperty(name = "locationTypeDescription")
    private String locationTypeDescription;


    public String getObjectTypeCode() {
        return objectTypeCode;
    }

    public void setObjectTypeCode(String objectTypeCode) {
        this.objectTypeCode = objectTypeCode;
    }

    public String getLocationTypeDescription() {
        return locationTypeDescription;
    }

    public void setLocationTypeDescription(String locationTypeDescription) {
        this.locationTypeDescription = locationTypeDescription;
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

    public String getLocationId() {
        return locationId;
    }

    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }

    public String getLocationDescription() {
        return locationDescription;
    }

    public void setLocationDescription(String locationDescription) {
        this.locationDescription = locationDescription;
    }

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public String getSourceSystem() {
        return sourceSystem;
    }

    public void setSourceSystem(String sourceSystem) {
        this.sourceSystem = sourceSystem;
    }

    public String getExternalId() {
        return externalId;
    }

    public void setExternalId(String externalId) {
        this.externalId = externalId;
    }

    public String getObjectTypeDescription() {
        return objectTypeDescription;
    }

    public void setObjectTypeDescription(String objectTypeDescription) {
        this.objectTypeDescription = objectTypeDescription;
    }

    public String getFormattedAddress() {
        return formattedAddress;
    }

    public void setFormattedAddress(String formattedAddress) {
        this.formattedAddress = formattedAddress;
    }
}
