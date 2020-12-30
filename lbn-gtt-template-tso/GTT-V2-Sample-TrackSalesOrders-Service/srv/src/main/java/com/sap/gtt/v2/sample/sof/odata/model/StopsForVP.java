package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.io.Serializable;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = StopsForVP.ENTITY_NAME)
@EdmEntitySet(name = StopsForVP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class StopsForVP implements Serializable {
    public static final String ENTITY_NAME = "ShipmentStopsForVP";
    public static final String ENTITY_SET_NAME = "ShipmentStopsForVP";

    @EdmKey
    @EdmProperty(name = "stopId", facets = @EdmFacets(maxLength = 10))
    private String stopId;

    @EdmProperty(name = "ordinalNumber")
    private Integer ordinalNumber;

    @EdmProperty(name = "locationId", facets = @EdmFacets(maxLength = 10))
    private String locationId;

    @EdmProperty(name = "locationType_code", facets = @EdmFacets(maxLength = 4))
    @SerializedName("locationType_code")
    private String locationTypeCode;

    @EdmNavigationProperty(name = "locationType", toType = LocationType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private LocationType locationType;
    private String locationAltKey;
    private LocationDTO location;

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

    public String getStopId() {
        return stopId;
    }

    public void setStopId(String stopId) {
        this.stopId = stopId;
    }

    public Integer getOrdinalNumber() {
        return ordinalNumber;
    }

    public void setOrdinalNumber(Integer ordinalNumber) {
        this.ordinalNumber = ordinalNumber;
    }

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public String getLocationId() {
        return locationId;
    }

    public void setLocationId(String locationId) {
        this.locationId = locationId;
    }

    public LocationType getLocationType() {
        return locationType;
    }

    public void setLocationType(LocationType locationType) {
        this.locationType = locationType;
    }
}
