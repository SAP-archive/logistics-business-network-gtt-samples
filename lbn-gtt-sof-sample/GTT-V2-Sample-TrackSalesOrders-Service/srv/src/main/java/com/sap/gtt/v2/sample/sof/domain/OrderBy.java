package com.sap.gtt.v2.sample.sof.domain;

public class OrderBy {
    private String orderField;
    private String sequence;
    public OrderBy() {}

    public OrderBy(String orderField, String sequence) {
        this.orderField = orderField;
        this.sequence = sequence;
    }

    public String getOrderField() {
        return orderField;
    }

    public void setOrderField(String orderField) {
        this.orderField = orderField;
    }

    public String getSequence() {
        return sequence;
    }

    public void setSequence(String sequence) {
        this.sequence = sequence;
    }
}
