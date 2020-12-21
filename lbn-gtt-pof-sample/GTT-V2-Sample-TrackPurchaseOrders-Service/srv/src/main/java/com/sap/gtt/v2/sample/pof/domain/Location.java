package com.sap.gtt.v2.sample.pof.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.math.BigDecimal;
import java.util.UUID;

public class Location {

    private String objectTypeCode;

    private String locationDescription;

    private String addressTimeZone;

    private String countryCode;

    private String regionCode;

    private String cityName;

    private String streetName;

    private String houseNumber;

    private String houseNumberSupplementText;

    private String postalCode;

    private String number;

    private String numberExtension;

    private String emailAddress;

    private transient BigDecimal longitude;

    private transient BigDecimal latitude;

    private UUID id;
    private UUID tenantId;
    private String locationId;
    private String locationAltKey;
    private String sourceSystem;
    private String externalId;
    private String locationTypeCode;
    private String locationTypeDescription;
    private String objectTypeDescription;
    private String regionName;
    private String countryName;
    private String addressDetail;
    private String formattedAddress;

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

    public String getAddressTimeZone() {
        return addressTimeZone;
    }

    public void setAddressTimeZone(String addressTimeZone) {
        this.addressTimeZone = addressTimeZone;
    }

    public String getCountryCode() {
        return countryCode;
    }

    public void setCountryCode(String countryCode) {
        this.countryCode = countryCode;
    }

    public String getRegionCode() {
        return regionCode;
    }

    public void setRegionCode(String regionCode) {
        this.regionCode = regionCode;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public String getStreetName() {
        return streetName;
    }

    public void setStreetName(String streetName) {
        this.streetName = streetName;
    }

    public String getHouseNumber() {
        return houseNumber;
    }

    public void setHouseNumber(String houseNumber) {
        this.houseNumber = houseNumber;
    }

    public String getHouseNumberSupplementText() {
        return houseNumberSupplementText;
    }

    public void setHouseNumberSupplementText(String houseNumberSupplementText) {
        this.houseNumberSupplementText = houseNumberSupplementText;
    }

    public String getPostalCode() {
        return postalCode;
    }

    public void setPostalCode(String postalCode) {
        this.postalCode = postalCode;
    }

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public String getNumberExtension() {
        return numberExtension;
    }

    public void setNumberExtension(String numberExtension) {
        this.numberExtension = numberExtension;
    }

    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(String emailAddress) {
        this.emailAddress = emailAddress;
    }

    public BigDecimal getLongitude() {
        return longitude;
    }

    public void setLongitude(BigDecimal longitude) {
        this.longitude = longitude;
        /*this.longitudeStr = longitude.toPlainString();*/
    }

    public BigDecimal getLatitude() {
        return latitude;
    }

    public void setLatitude(BigDecimal latitude) {
        this.latitude = latitude;
        /*this.latitudeStr = latitude.toPlainString();*/
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    @JsonIgnore
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

    public String getLocationTypeCode() {
        return locationTypeCode;
    }

    public void setLocationTypeCode(String locationTypeCode) {
        this.locationTypeCode = locationTypeCode;
    }

    public String getLocationTypeDescription() {
        return locationTypeDescription;
    }

    public void setLocationTypeDescription(String locationTypeDescription) {
        this.locationTypeDescription = locationTypeDescription;
    }

    public String getObjectTypeDescription() {
        return objectTypeDescription;
    }

    public void setObjectTypeDescription(String objectTypeDescription) {
        this.objectTypeDescription = objectTypeDescription;
    }

    public String getRegionName() {
        return regionName;
    }

    public void setRegionName(String regionName) {
        this.regionName = regionName;
    }

    public String getCountryName() {
        return countryName;
    }

    public void setCountryName(String countryName) {
        this.countryName = countryName;
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
