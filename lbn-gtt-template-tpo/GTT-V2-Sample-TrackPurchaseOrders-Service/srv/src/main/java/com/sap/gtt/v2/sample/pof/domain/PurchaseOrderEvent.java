package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class PurchaseOrderEvent extends EventForWrite {
    private String purchaseOrderNo;
    private BigDecimal completionValue;
    private String isDelayed;
    private BigDecimal delayedValue;
    private BigDecimal completedAndLateValue;

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

    public String getIsDelayed() {
        return isDelayed;
    }

    public void setIsDelayed(String isDelayed) {
        this.isDelayed = isDelayed;
    }

    public BigDecimal getDelayedValue() {
        return delayedValue;
    }

    public void setDelayedValue(BigDecimal delayedValue) {
        this.delayedValue = delayedValue;
    }

    public BigDecimal getCompletedAndLateValue() {
        return completedAndLateValue;
    }

    public void setCompletedAndLateValue(BigDecimal completedAndLateValue) {
        this.completedAndLateValue = completedAndLateValue;
    }
}
