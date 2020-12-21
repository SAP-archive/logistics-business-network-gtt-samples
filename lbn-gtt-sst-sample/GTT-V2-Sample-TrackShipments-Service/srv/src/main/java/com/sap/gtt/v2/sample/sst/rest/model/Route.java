package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import java.util.List;

/**
 * @author Aliaksandr Miron
 */
public class Route {

    private List<StopsForVp> stopsForVp;

    @JsonProperty("actualRoute")
    private List<ActualSpot> actualSpots;

    @JsonProperty("plannedRoute")
    private List<PlannedSpot> plannedSpots;

    private CurrentLocation currentLocation;

    private boolean toConnectPlannedAndActualRoute;

    private boolean hasLocationWithInvalidGeo;

    private boolean hasActualEventWithInvalidGeo;

    public List<StopsForVp> getStopsForVp() {
        return stopsForVp;
    }

    public void setStopsForVp(List<StopsForVp> stopsForVp) {
        this.stopsForVp = stopsForVp;
    }

    public List<ActualSpot> getActualSpots() {
        return actualSpots;
    }

    public void setActualSpots(List<ActualSpot> actualSpots) {
        this.actualSpots = actualSpots;
    }

    public List<PlannedSpot> getPlannedSpots() {
        return plannedSpots;
    }

    public void setPlannedSpots(List<PlannedSpot> plannedSpots) {
        this.plannedSpots = plannedSpots;
    }

    public CurrentLocation getCurrentLocation() {
        return currentLocation;
    }

    public void setCurrentLocation(CurrentLocation currentLocation) {
        this.currentLocation = currentLocation;
    }

    public boolean isToConnectPlannedAndActualRoute() {
        return toConnectPlannedAndActualRoute;
    }

    public void setToConnectPlannedAndActualRoute(boolean toConnectPlannedAndActualRoute) {
        this.toConnectPlannedAndActualRoute = toConnectPlannedAndActualRoute;
    }

    public boolean isHasLocationWithInvalidGeo() {
        return hasLocationWithInvalidGeo;
    }

    public void setHasLocationWithInvalidGeo(boolean hasLocationWithInvalidGeo) {
        this.hasLocationWithInvalidGeo = hasLocationWithInvalidGeo;
    }

    public boolean isHasActualEventWithInvalidGeo() {
        return hasActualEventWithInvalidGeo;
    }

    public void setHasActualEventWithInvalidGeo(boolean hasActualEventWithInvalidGeo) {
        this.hasActualEventWithInvalidGeo = hasActualEventWithInvalidGeo;
    }
}
