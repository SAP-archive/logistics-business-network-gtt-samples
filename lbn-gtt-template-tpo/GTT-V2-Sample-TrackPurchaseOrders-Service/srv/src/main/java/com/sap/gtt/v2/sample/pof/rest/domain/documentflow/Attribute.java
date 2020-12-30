package com.sap.gtt.v2.sample.pof.rest.domain.documentflow;

import com.fasterxml.jackson.annotation.JsonInclude;

public class Attribute {

    private String propertyName;
    private String value;
    private Integer group;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String uom;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String valueStatus;


    public Attribute(String propertyName, String value, String uom, String valueStatus, Integer group) {
        this.propertyName = propertyName;
        this.value = value;
        this.uom = uom;
        this.valueStatus = valueStatus;
        this.group = group;
    }

    public String getPropertyName() {
        return propertyName;
    }

    public void setPropertyName(String propertyName) {
        this.propertyName = propertyName;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getUom() {
        return uom;
    }

    public void setUom(String uom) {
        this.uom = uom;
    }

    public String getValueStatus() {
        return valueStatus;
    }

    public void setValueStatus(String valueStatus) {
        this.valueStatus = valueStatus;
    }

    public Integer getGroup() {
        return group;
    }

    public void setGroup(Integer group) {
        this.group = group;
    }

}
