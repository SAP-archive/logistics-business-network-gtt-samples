package com.sap.gtt.v2.sample.sst.rest.model;

/**
 * @author Min Li
 */
public class TimeZone {

    private String timeZoneCode;
    private String description;
    private String offset;

    public String getTimeZoneCode() {
        return timeZoneCode;
    }

    public void setTimeZoneCode(String timeZoneCode) {
        this.timeZoneCode = timeZoneCode;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getOffset() {
        return offset;
    }

    public void setOffset(String offset) {
        this.offset = offset;
    }
}
