package com.sap.gtt.v2.sample.sst.rest.model;

import com.sap.gtt.v2.sample.sst.odata.model.Location;

/**
 * @author Aliaksandr Miron
 */
public class NextStop {

    private Location location;

    private String estimatedArrivalTime;

    private String plannedBusinessTimestamp;

    public Location getLocation() {
        return location;
    }

    public void setLocation(Location location) {
        this.location = location;
    }

    public String getEstimatedArrivalTime() {
        return estimatedArrivalTime;
    }

    public void setEstimatedArrivalTime(String estimatedArrivalTime) {
        this.estimatedArrivalTime = estimatedArrivalTime;
    }

    public String getPlannedBusinessTimestamp() {
        return plannedBusinessTimestamp;
    }

    public void setPlannedBusinessTimestamp(String plannedBusinessTimestamp) {
        this.plannedBusinessTimestamp = plannedBusinessTimestamp;
    }
}
