package com.sap.gtt.v2.sample.sst.rest.model.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * {@link CarrierReferenceDocumentDto} is a DTO of {@link com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument}.
 *
 * @author Aliaksandr Miron
 */
public class CarrierReferenceDocumentDto {

    private String docId;

    @JsonProperty("docType_code")
    private String docTypeCode;

    private String shipmentNo;

    public String getDocId() {
        return docId;
    }

    public void setDocId(String docId) {
        this.docId = docId;
    }

    public String getDocTypeCode() {
        return docTypeCode;
    }

    public void setDocTypeCode(String docTypeCode) {
        this.docTypeCode = docTypeCode;
    }

    public String getShipmentNo() {
        return shipmentNo;
    }

    public void setShipmentNo(String shipmentNo) {
        this.shipmentNo = shipmentNo;
    }
}
