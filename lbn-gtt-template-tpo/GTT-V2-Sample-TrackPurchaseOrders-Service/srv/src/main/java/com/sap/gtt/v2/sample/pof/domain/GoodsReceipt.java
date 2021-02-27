package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class GoodsReceipt extends Event {

    private BigDecimal quantity;

    private String reversal;

    public BigDecimal getQuantity() {
        return quantity;
    }

    public void setQuantity(BigDecimal quantity) {
        this.quantity = quantity;
    }

    public String getReversal() {
        return reversal;
    }

    public void setReversal(String reversal) {
        this.reversal = reversal;
    }
}
