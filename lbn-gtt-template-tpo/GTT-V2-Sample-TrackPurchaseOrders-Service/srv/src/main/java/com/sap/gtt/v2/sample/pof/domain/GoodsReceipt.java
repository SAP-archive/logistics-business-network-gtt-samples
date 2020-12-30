package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class GoodsReceipt extends Event {

    private BigDecimal quantity;

    public BigDecimal getQuantity() {
        return quantity;
    }

    public void setQuantity(BigDecimal quantity) {
        this.quantity = quantity;
    }
}
