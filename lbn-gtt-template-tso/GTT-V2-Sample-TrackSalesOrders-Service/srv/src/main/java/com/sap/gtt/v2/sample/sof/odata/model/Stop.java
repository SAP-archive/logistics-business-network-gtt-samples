package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = Stop.ENTITY_NAME)
@EdmEntitySet(name = Stop.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class Stop {
    public static final String ENTITY_NAME = "ShipmentStops";
    public static final String ENTITY_SET_NAME = "ShipmentStops";

    @EdmKey
    @EdmProperty(name = "stopId", facets = @EdmFacets(maxLength = 10))
    private String stopId;

    @EdmProperty(name = "ordinalNumber")
    private Integer ordinalNumber;

    @EdmProperty(name = "warehouse", facets = @EdmFacets(maxLength = 3))
    private String warehouse;

    @EdmProperty(name = "gate", facets = @EdmFacets(maxLength = 3))
    private String gate;

    @EdmProperty(name = "loadingPoint", facets = @EdmFacets(maxLength = 2))
    private String loadingPoint;

    @EdmProperty(name = "unloadingPoint", facets = @EdmFacets(maxLength = 25))
    private String unloadingPoint;

    @EdmProperty(name = "warehouseDescription", facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;

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

    public String getWarehouse() {
        return warehouse;
    }

    public void setWarehouse(String warehouse) {
        this.warehouse = warehouse;
    }

    public String getGate() {
        return gate;
    }

    public void setGate(String gate) {
        this.gate = gate;
    }

    public String getLoadingPoint() {
        return loadingPoint;
    }

    public void setLoadingPoint(String loadingPoint) {
        this.loadingPoint = loadingPoint;
    }

    public String getUnloadingPoint() {
        return unloadingPoint;
    }

    public void setUnloadingPoint(String unloadingPoint) {
        this.unloadingPoint = unloadingPoint;
    }

    public String getWarehouseDescription() {
        return warehouseDescription;
    }

    public void setWarehouseDescription(String warehouseDescription) {
        this.warehouseDescription = warehouseDescription;
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
