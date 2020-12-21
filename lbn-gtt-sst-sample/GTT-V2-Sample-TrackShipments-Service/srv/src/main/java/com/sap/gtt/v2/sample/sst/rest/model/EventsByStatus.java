package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Aliaksandr Miron
 */
public class EventsByStatus {

    @JsonProperty("eventStatus_code")
    private String statusCode;

    private Integer count;

    public EventsByStatus(final String statusCode, final Integer count) {
        this.statusCode = statusCode;
        this.count = count;
    }

    public String getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(String statusCode) {
        this.statusCode = statusCode;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }
}
