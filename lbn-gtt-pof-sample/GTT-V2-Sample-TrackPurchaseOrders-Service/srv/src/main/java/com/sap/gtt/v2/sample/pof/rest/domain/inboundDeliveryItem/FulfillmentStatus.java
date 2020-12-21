package com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import com.google.gson.annotations.SerializedName;

@JsonPropertyOrder({"eventStatus_code", "count"})
public class FulfillmentStatus {
    @JsonProperty("eventStatus_code")
    @SerializedName("eventStatus_code")
    private String eventStatusCode;
    private Integer count;

    public FulfillmentStatus(String eventStatusCode, Integer count) {
        this.eventStatusCode = eventStatusCode;
        this.count = count;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }

    @Override
    public String toString() {
        return "FulfillmentStatus{" +
                "eventStatusCode='" + eventStatusCode + '\'' +
                ", count=" + count +
                '}';
    }
}
