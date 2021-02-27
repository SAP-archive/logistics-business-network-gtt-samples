package com.sap.gtt.v2.sample.sst.rest.model.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;

/**
 * @author Aliaksandr Miron
 */
public class DeliveryItemDto {

    private String id;

    private String deliveryNo;

    private String itemNo;

    private BigDecimal orderQuantity;

    private String quantityUoM;

    private String materialNo;

    private String materialDescription;

    private String eventStatusCode;

    private String altKey;

    private String eventMatchKey;

    private Boolean isInFreightUnit;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getDeliveryNo() {
        return deliveryNo;
    }

    public void setDeliveryNo(String deliveryNo) {
        this.deliveryNo = deliveryNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public BigDecimal getOrderQuantity() {
        return orderQuantity;
    }

    public void setOrderQuantity(BigDecimal orderQuantity) {
        this.orderQuantity = orderQuantity;
    }

    public String getQuantityUoM() {
        return quantityUoM;
    }

    public void setQuantityUoM(String quantityUoM) {
        this.quantityUoM = quantityUoM;
    }

    public String getMaterialNo() {
        return materialNo;
    }

    public void setMaterialNo(String materialNo) {
        this.materialNo = materialNo;
    }

    public String getMaterialDescription() {
        return materialDescription;
    }

    public void setMaterialDescription(String materialDescription) {
        this.materialDescription = materialDescription;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }

    @JsonProperty("isInFreightUnit")
    public Boolean getInFreightUnit() {
        return isInFreightUnit;
    }

    public void setInFreightUnit(Boolean inFreightUnit) {
        isInFreightUnit = inFreightUnit;
    }
}
