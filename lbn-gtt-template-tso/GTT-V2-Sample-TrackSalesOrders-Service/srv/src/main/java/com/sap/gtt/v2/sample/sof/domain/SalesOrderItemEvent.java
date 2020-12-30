package com.sap.gtt.v2.sample.sof.domain;

import java.math.BigDecimal;


public class SalesOrderItemEvent extends EventForWrite {
    private String salesOrderNo;
    private String itemNo;
    private BigDecimal delayedQuantity;
    private BigDecimal completionQuantity;
    private BigDecimal completedAndLateQuantity;
    private Boolean isCompleted;
    private Boolean isDelayed;

    public String getSalesOrderNo() {
        return salesOrderNo;
    }

    public void setSalesOrderNo(String salesOrderNo) {
        this.salesOrderNo = salesOrderNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public BigDecimal getDelayedQuantity() {
        return delayedQuantity;
    }

    public void setDelayedQuantity(BigDecimal delayedQuantity) {
        this.delayedQuantity = delayedQuantity;
    }

    public BigDecimal getCompletionQuantity() {
        return completionQuantity;
    }

    public void setCompletionQuantity(BigDecimal completionQuantity) {
        this.completionQuantity = completionQuantity;
    }

    public BigDecimal getCompletedAndLateQuantity() {
        return completedAndLateQuantity;
    }

    public void setCompletedAndLateQuantity(BigDecimal completedAndLateQuantity) {
        this.completedAndLateQuantity = completedAndLateQuantity;
    }

    public Boolean getCompleted() {
        return isCompleted;
    }

    public void setCompleted(Boolean completed) {
        isCompleted = completed;
    }

    public Boolean getDelayed() {
        return isDelayed;
    }

    public void setDelayed(Boolean delayed) {
        isDelayed = delayed;
    }
}
