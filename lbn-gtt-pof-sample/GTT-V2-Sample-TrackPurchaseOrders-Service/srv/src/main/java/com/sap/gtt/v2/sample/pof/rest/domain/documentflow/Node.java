package com.sap.gtt.v2.sample.pof.rest.domain.documentflow;

import java.util.List;
import java.util.UUID;

public class Node {

    private UUID id;
    private Integer key;
    private String title;
    private Integer group;
    private String status;
    private List<Attribute> attributes;
    private String trackingIdType;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public Integer getKey() {
        return key;
    }

    public void setKey(Integer key) {
        this.key = key;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Integer getGroup() {
        return group;
    }

    public void setGroup(Integer group) {
        this.group = group;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public List<Attribute> getAttributes() {
        return attributes;
    }

    public void setAttributes(List<Attribute> attributes) {
        this.attributes = attributes;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }
}
