package com.sap.gtt.v2.sample.sof.domain;

import java.math.BigDecimal;


public class SalesOrderEvent extends EventForWrite {
    private String salesOrderNo;
    private BigDecimal delayedValue;
    private BigDecimal completionValue;
    private BigDecimal completedAndLateValue;
    private Boolean isCompleted;
    private Boolean isDelayed;

    public String getSalesOrderNo() {
        return salesOrderNo;
    }

    public void setSalesOrderNo(String salesOrderNo) {
        this.salesOrderNo = salesOrderNo;
    }

    public BigDecimal getDelayedValue() {
        return delayedValue;
    }

    public void setDelayedValue(BigDecimal delayedValue) {
        this.delayedValue = delayedValue;
    }

    public BigDecimal getCompletionValue() {
        return completionValue;
    }

    public void setCompletionValue(BigDecimal completionValue) {
        this.completionValue = completionValue;
    }

    public BigDecimal getCompletedAndLateValue() {
        return completedAndLateValue;
    }

    public void setCompletedAndLateValue(BigDecimal completedAndLateValue) {
        this.completedAndLateValue = completedAndLateValue;
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
