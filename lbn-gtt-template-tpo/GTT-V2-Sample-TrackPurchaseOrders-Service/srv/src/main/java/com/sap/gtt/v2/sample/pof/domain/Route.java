package com.sap.gtt.v2.sample.pof.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.StopsForVP;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.*;

public class Route implements Serializable {
    private List<PlannedSpot> plannedSpots;

    private CurrentLocation currentLocation;

    private PlannedEvent nextStopEvent;

    private String groupId;

    private List<ActualSpot> actualSpots;

    private String altKey;

    private String transportationModeCode;

    private UUID firstPlannedEventId;

    private String executionStatusCode;

    @JsonProperty("stops")
    private List<StopsForVP> stopsForVPS;

    private Set<String> locationAltKey;

    private String destinationLocationAltKey;

    private LocationDTO destinationLocation;

    private ValidCoordinates actualSpotsValidation;

    private ValidCoordinates plannedLocationsValidation;

    public Route() {
        locationAltKey = new HashSet<>();
        plannedSpots = new ArrayList<>();
        actualSpots = new ArrayList<>();
        actualSpotsValidation = new ValidCoordinates();
        plannedLocationsValidation = new ValidCoordinates();
        stopsForVPS = new ArrayList<>();
    }

    public ValidCoordinates getActualSpotsValidation() {
        return actualSpotsValidation;
    }

    public void setActualSpotsValidation(ValidCoordinates actualSpotsValidation) {
        this.actualSpotsValidation = actualSpotsValidation;
    }
    public void setActualSpotsMissing() {
        actualSpotsValidation.setMissing(true);
        actualSpotsValidation.setValid(false);
    }
    public void setActualSpotsInvalid() {
        actualSpotsValidation.setInvalid(true);
        actualSpotsValidation.setValid(false);
    }
    public void setPlannedLocationMissing() {
        plannedLocationsValidation.setMissing(true);
        plannedLocationsValidation.setValid(false);
    }
    public void setPlannedLocationInvalid() {
        plannedLocationsValidation.setInvalid(true);
        plannedLocationsValidation.setValid(false);
    }
    public ValidCoordinates getPlannedLocationsValidation() {
        return plannedLocationsValidation;
    }

    public void setPlannedLocationsValidation(ValidCoordinates plannedLocationsValidation) {
        this.plannedLocationsValidation = plannedLocationsValidation;
    }

    public String getDestinationLocationAltKey() {
        return destinationLocationAltKey;
    }

    public void setDestinationLocationAltKey(String destinationLocationAltKey) {
        this.destinationLocationAltKey = destinationLocationAltKey;
    }

    public LocationDTO getDestinationLocation() {
        return destinationLocation;
    }

    public void setDestinationLocation(LocationDTO destinationLocation) {
        this.destinationLocation = destinationLocation;
    }

    public Set<String> getLocationAltKey() {
        return locationAltKey;
    }

    public void setLocationAltKey(Set<String> locationAltKey) {
        this.locationAltKey = locationAltKey;
    }
    @JsonIgnore
    public void addLocationAltKey(String locationAltKey) {
        if(StringUtils.isNotBlank(locationAltKey)) {
            this.getLocationAltKey().add(locationAltKey);
        }
    }
    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public List<StopsForVP> getStopsForVPS() {
        return stopsForVPS;
    }

    public void setStopsForVPS(List<StopsForVP> stopsForVPS) {
        this.stopsForVPS = stopsForVPS;
    }
    public UUID getFirstPlannedEventId() {
        return firstPlannedEventId;
    }

    public void setFirstPlannedEventId(UUID firstPlannedEventId) {
        this.firstPlannedEventId = firstPlannedEventId;
    }
    public String getGroupId() {
        return groupId;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getTransportationModeCode() {
        return transportationModeCode;
    }

    public void setTransportationModeCode(String transportationModeCode) {
        this.transportationModeCode = transportationModeCode;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public PlannedEvent getNextStopEvent() {
        return nextStopEvent;
    }

    public void setNextStopEvent(PlannedEvent nextStopEvent) {
        this.nextStopEvent = nextStopEvent;
    }

    public List<PlannedSpot> getPlannedSpots() {
        return plannedSpots;
    }
    public List<ActualSpot> getActualSpots() {
        return actualSpots;
    }
    public void setActualSpots(List<ActualSpot> actualSpots) {
        this.actualSpots = actualSpots;
    }
    public void addActualSpot(ActualSpot actualSpot) {
        this.getActualSpots().add(actualSpot);
    }
    public void addActualSpots(List<ActualSpot> actualSpots) {
        this.getActualSpots().addAll(actualSpots);
    }
    public void setPlannedSpots(List<PlannedSpot> plannedSpots) {
        this.plannedSpots = plannedSpots;
    }
    public void addPlannedSpot(PlannedSpot spot) {
        this.getPlannedSpots().add(spot);
    }

    public void addPlannedSpots(List<PlannedSpot> spots) {
        this.getPlannedSpots().addAll(spots);
    }

    @JsonIgnore
    public ActualSpot getLastActualSpot() {
        List<ActualSpot> spots = this.getActualSpots();
        if(spots.isEmpty()) {
            return null;
        }
        return spots.get(spots.size()-1);
    }
    public CurrentLocation getCurrentLocation() {
        return currentLocation;
    }

    public void setCurrentLocation(CurrentLocation currentLocation) {
        this.currentLocation = currentLocation;
    }
}
