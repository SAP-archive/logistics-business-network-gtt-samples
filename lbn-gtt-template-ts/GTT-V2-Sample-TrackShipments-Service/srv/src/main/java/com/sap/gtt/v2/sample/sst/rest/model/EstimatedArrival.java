package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.sap.gtt.v2.sample.sst.common.config.JsonDateTimeSerializer;

/**
 * @author Aliaksandr Miron
 */
public class EstimatedArrival {

    private String stopId;

    @JsonSerialize(using = JsonDateTimeSerializer.class)
    private Long estimatedArrivalTime;

    private String estimatedArrivalTimeZone;

    private String eventStatusCode;

    public String getStopId() {
        return stopId;
    }

    public void setStopId(String stopId) {
        this.stopId = stopId;
    }

    public Long getEstimatedArrivalTime() {
        return estimatedArrivalTime;
    }

    public void setEstimatedArrivalTime(Long estimatedArrivalTime) {
        this.estimatedArrivalTime = estimatedArrivalTime;
    }

    public String getEstimatedArrivalTimeZone() {
        return estimatedArrivalTimeZone;
    }

    public void setEstimatedArrivalTimeZone(String estimatedArrivalTimeZone) {
        this.estimatedArrivalTimeZone = estimatedArrivalTimeZone;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }
}
