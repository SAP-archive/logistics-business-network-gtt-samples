package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class PurchaseOrderEvent extends EventForWrite {
    private String purchaseOrderNo;
    private BigDecimal completionValue;

    public String getPurchaseOrderNo() {
        return purchaseOrderNo;
    }

    public void setPurchaseOrderNo(String purchaseOrderNo) {
        this.purchaseOrderNo = purchaseOrderNo;
    }

    public BigDecimal getCompletionValue() {
        return completionValue;
    }

    public void setCompletionValue(BigDecimal completionValue) {
        this.completionValue = completionValue;
    }
}
