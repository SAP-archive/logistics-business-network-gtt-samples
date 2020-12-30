package com.sap.gtt.v2.sample.pof.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;

import java.io.Serializable;

public class EstimatedArrival implements Serializable {
    private String stopId;
    @JsonProperty("estimatedArrivalTime")
    private String eta;
    private String estimatedArrivalTimeZone;
    @JsonIgnore
    private Long estimatedArrivalTime;

    public String getEta() {
        return POFUtils.getTimeStr(estimatedArrivalTime);
    }

    public void setEta(String eta) {
        this.eta = eta;
    }

    public Long getEstimatedArrivalTime() {
        return estimatedArrivalTime;
    }

    public void setEstimatedArrivalTime(Long estimatedArrivalTime) {
        this.estimatedArrivalTime = estimatedArrivalTime;
    }

    public String getStopId() {
        return stopId;
    }

    public void setStopId(String stopId) {
        this.stopId = stopId;
    }

    public String getEstimatedArrivalTimeZone() {
        return estimatedArrivalTimeZone;
    }

    public void setEstimatedArrivalTimeZone(String estimatedArrivalTimeZone) {
        this.estimatedArrivalTimeZone = estimatedArrivalTimeZone;
    }
}
