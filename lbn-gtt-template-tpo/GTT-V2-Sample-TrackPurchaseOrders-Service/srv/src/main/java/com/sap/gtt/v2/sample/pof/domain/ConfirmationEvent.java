package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class ConfirmationEvent extends Event {
    private BigDecimal quantity;
    private String confirmType;

    public BigDecimal getQuantity() {
        return quantity;
    }

    public void setQuantity(BigDecimal quantity) {
        this.quantity = quantity;
    }

    public String getConfirmType() {
        return confirmType;
    }

    public void setConfirmType(String confirmType) {
        this.confirmType = confirmType;
    }
}
