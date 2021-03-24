package com.sap.gtt.v2.sample.pof.domain;

import java.math.BigDecimal;

public class PurchaseOrderItemEvent extends EventForWrite {
    private String purchaseOrderNo;
    private String itemNo;
    private BigDecimal completedQuantity;
    private BigDecimal completionValue;
    private String isDelayed;
    private BigDecimal delayedValue;
    private BigDecimal delayedQuantity;
    private BigDecimal completedAndLateValue;
    private BigDecimal completedAndLateQuantity;

    public String getPurchaseOrderNo() {
        return purchaseOrderNo;
    }

    public void setPurchaseOrderNo(String purchaseOrderNo) {
        this.purchaseOrderNo = purchaseOrderNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public BigDecimal getCompletedQuantity() {
        return completedQuantity;
    }

    public void setCompletedQuantity(BigDecimal completedQuantity) {
        this.completedQuantity = completedQuantity;
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

    public BigDecimal getDelayedQuantity() {
        return delayedQuantity;
    }

    public void setDelayedQuantity(BigDecimal delayedQuantity) {
        this.delayedQuantity = delayedQuantity;
    }

    public BigDecimal getCompletedAndLateValue() {
        return completedAndLateValue;
    }

    public void setCompletedAndLateValue(BigDecimal completedAndLateValue) {
        this.completedAndLateValue = completedAndLateValue;
    }

    public BigDecimal getCompletedAndLateQuantity() {
        return completedAndLateQuantity;
    }

    public void setCompletedAndLateQuantity(BigDecimal completedAndLateQuantity) {
        this.completedAndLateQuantity = completedAndLateQuantity;
    }
}
