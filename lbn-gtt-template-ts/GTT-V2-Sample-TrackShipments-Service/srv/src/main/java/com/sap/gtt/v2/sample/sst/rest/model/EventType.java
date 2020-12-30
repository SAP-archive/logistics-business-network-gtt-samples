package com.sap.gtt.v2.sample.sst.rest.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.annotations.SerializedName;

/**
 * @author Min Li
 */
public class EventType {

    private String target;

    private String name;

    @SerializedName("descr")
    @JsonProperty("descr")
    private String description;

    private Boolean isFromCoreModel;

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    @JsonProperty("isFromCoreModel")
    public Boolean getFromCoreModel() {
        return isFromCoreModel;
    }

    public void setFromCoreModel(Boolean fromCoreModel) {
        isFromCoreModel = fromCoreModel;
    }
}
