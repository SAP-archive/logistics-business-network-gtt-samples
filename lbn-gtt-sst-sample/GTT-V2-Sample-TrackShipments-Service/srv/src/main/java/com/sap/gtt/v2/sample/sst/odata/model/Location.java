package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;

import java.math.BigDecimal;
import java.util.UUID;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = Location.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class Location {

    public static final String ENTITY_SET_NAME = "Location";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "locationAltKey")
    private String locationAltKey;

    @EdmProperty(name = "tenantId")
    private UUID tenantId;

    @EdmProperty(name = "locationId")
    private String locationId;

    @EdmProperty(name = "sourceSystem")
    private String sourceSystem;

    @EdmProperty(name = "externalId")
    private String externalId;

    @EdmProperty(name = "longitude")
    private BigDecimal longitude;

    @EdmProperty(name = "latitude")
    private BigDecimal latitude;

    @EdmProperty(name = "locationTypeCode")
    private String locationTypeCode;

    @EdmProperty(name = "locationDescription")
    private String locationDescription;

    @EdmProperty(name = "locationTypeDescription")
    private String locationTypeDescription;

    @EdmProperty(name = "objectTypeCode")
    private String objectTypeCode;

    @EdmProperty(name = "objectTypeDescription")
    private String objectTypeDescription;

    @EdmProperty(name = "postalCode")
    private String postalCode;

    @EdmProperty(name = "houseNumber")
    private String houseNumber;

    @EdmProperty(name = "streetName")
    private String streetName;

    @EdmProperty(name = "cityName")
    private String cityName;

    @EdmProperty(name = "regionCode")
    private String regionCode;

    @EdmProperty(name = "regionName")
    private String regionName;

    @EdmProperty(name = "countryCode")
    private String countryCode;

    @EdmProperty(name = "countryName")
    private String countryName;

    @EdmProperty(name = "addressTimeZone")
    private String addressTimeZone;

    @EdmProperty(name = "timeZoneId")
    private String timeZoneId;

    @EdmProperty(name = "timeZoneDisplayName")
    private String timeZoneDisplayName;

    @EdmProperty(name = "addressDetail")
    private String addressDetail;

    @EdmProperty(name = "formattedAddress")
    private String formattedAddress;

    public String getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(String locationAltKey) {
        this.locationAltKey = locationAltKey;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public UUID getTenantId() {
        return tenantId;
    }

    public void setTenantId(UUID tenantId) {
        this.tenantId = tenantId;
    }

    public String getLocationId() {
        return locationId;
    }

    public void setLocationId(String locationId) {
        this.locationId = locationId;
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

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public String getLocationDescription() {
        return locationDescription;
    }

    public void setLocationDescription(String locationDescription) {
        this.locationDescription = locationDescription;
    }

    public String getLocationTypeDescription() {
        return locationTypeDescription;
    }

    public void setLocationTypeDescription(String locationTypeDescription) {
        this.locationTypeDescription = locationTypeDescription;
    }

    public String getObjectTypeCode() {
        return objectTypeCode;
    }

    public void setObjectTypeCode(String objectTypeCode) {
        this.objectTypeCode = objectTypeCode;
    }

    public String getObjectTypeDescription() {
        return objectTypeDescription;
    }

    public void setObjectTypeDescription(String objectTypeDescription) {
        this.objectTypeDescription = objectTypeDescription;
    }

    public String getPostalCode() {
        return postalCode;
    }

    public void setPostalCode(String postalCode) {
        this.postalCode = postalCode;
    }

    public String getHouseNumber() {
        return houseNumber;
    }

    public void setHouseNumber(String houseNumber) {
        this.houseNumber = houseNumber;
    }

    public String getStreetName() {
        return streetName;
    }

    public void setStreetName(String streetName) {
        this.streetName = streetName;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public String getRegionCode() {
        return regionCode;
    }

    public void setRegionCode(String regionCode) {
        this.regionCode = regionCode;
    }

    public String getRegionName() {
        return regionName;
    }

    public void setRegionName(String regionName) {
        this.regionName = regionName;
    }

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public String getCountryName() {
        return countryName;
    }

    public void setCountryName(String countryName) {
        this.countryName = countryName;
    }

    public String getAddressTimeZone() {
        return addressTimeZone;
    }

    public void setAddressTimeZone(String addressTimeZone) {
        this.addressTimeZone = addressTimeZone;
    }

    public String getTimeZoneId() {
        return timeZoneId;
    }

    public void setTimeZoneId(String timeZoneId) {
        this.timeZoneId = timeZoneId;
    }

    public String getTimeZoneDisplayName() {
        return timeZoneDisplayName;
    }

    public void setTimeZoneDisplayName(String timeZoneDisplayName) {
        this.timeZoneDisplayName = timeZoneDisplayName;
    }

    public String getAddressDetail() {
        return addressDetail;
    }

    public void setAddressDetail(String addressDetail) {
        this.addressDetail = addressDetail;
    }

    public String getFormattedAddress() {
        return formattedAddress;
    }

    public void setFormattedAddress(String formattedAddress) {
        this.formattedAddress = formattedAddress;
    }
}
