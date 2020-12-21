package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmFacets;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = StopsForVp.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class StopsForVp {

    public static final String ENTITY_SET_NAME = "ShipmentStopsForVP";

    @EdmKey
    @EdmProperty(name = "stopId", facets = @EdmFacets(maxLength = 255))
    private String stopId;

    @EdmProperty(name = "ordinalNo")
    private Integer ordinalNo;

    @EdmProperty(name = "locationId", facets = @EdmFacets(maxLength = 50))
    private String locationId;

    @EdmProperty(name = "locationType_code", facets = @EdmFacets(maxLength = 20))
    @SerializedName("locationType_code")
    private String locationTypeCode;

    @EdmNavigationProperty(name = "locationType", toType = LocationType.class, toMultiplicity = ONE)
    private LocationType locationType;

    private Location location;

    private EstimatedArrival estimatedArrival;

    public String getStopId() {
        return stopId;
    }

    public void setStopId(String stopId) {
        this.stopId = stopId;
    }

    public Integer getOrdinalNo() {
        return ordinalNo;
    }

    public void setOrdinalNo(Integer ordinalNo) {
        this.ordinalNo = ordinalNo;
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

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public EstimatedArrival getEstimatedArrival() {
        return estimatedArrival;
    }

    public void setEstimatedArrival(EstimatedArrival estimatedArrival) {
        this.estimatedArrival = estimatedArrival;
    }
}
