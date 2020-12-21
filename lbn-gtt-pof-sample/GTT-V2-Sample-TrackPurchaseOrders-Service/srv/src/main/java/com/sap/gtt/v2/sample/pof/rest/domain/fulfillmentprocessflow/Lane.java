package com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow;

import java.math.BigDecimal;

public class Lane {

    private String id;
    private String name;
    private BigDecimal count;
    private BigDecimal total;
    private Integer position;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public BigDecimal getCount() {
        return count;
    }

    public void setCount(BigDecimal count) {
        this.count = count;
    }

    public BigDecimal getTotal() {
        return total;
    }

    public void setTotal(BigDecimal total) {
        this.total = total;
    }

    public Integer getPosition() {
        return position;
    }

    public void setPosition(Integer position) {
        this.position = position;
    }
}
