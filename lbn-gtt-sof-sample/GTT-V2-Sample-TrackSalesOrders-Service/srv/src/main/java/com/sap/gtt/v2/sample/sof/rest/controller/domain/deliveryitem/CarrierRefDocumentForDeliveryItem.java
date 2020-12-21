package com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.gson.annotations.SerializedName;

import java.util.UUID;

public class CarrierRefDocumentForDeliveryItem {

    @SerializedName("docType_code")
    @JsonProperty("docType_code")
    private String docTypeCode;
    private String docId;
    private String shipmentNo;
    private UUID shipmentId;

    public String getDocTypeCode() {
        return docTypeCode;
    }

    public void setDocTypeCode(String docTypeCode) {
        this.docTypeCode = docTypeCode;
    }

    public String getDocId() {
        return docId;
    }

    public void setDocId(String docId) {
        this.docId = docId;
    }

    public String getShipmentNo() {
        return shipmentNo;
    }

    public void setShipmentNo(String shipmentNo) {
        this.shipmentNo = shipmentNo;
    }

    public UUID getShipmentId() {
        return shipmentId;
    }

    public void setShipmentId(UUID shipmentId) {
        this.shipmentId = shipmentId;
    }

    @Override
    public String toString() {
        return "CarrierRefDocumentForDeliveryItem{" +
                "docTypeCode='" + docTypeCode + '\'' +
                ", docId='" + docId + '\'' +
                ", shipmentNo='" + shipmentNo + '\'' +
                ", shipmentId=" + shipmentId +
                '}';
    }
}
