package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = CarrierRefDocument.ENTITY_NAME)
@EdmEntitySet(name = CarrierRefDocument.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class CarrierRefDocument {
    public static final String ENTITY_NAME = "ShipmentCarrierRefDocuments";
    public static final String ENTITY_SET_NAME = "ShipmentCarrierRefDocuments";

    @EdmKey
    @EdmProperty(name = "docType_code", facets = @EdmFacets(maxLength = 20))
    @SerializedName("docType_code")
    private String docTypeCode;

    @EdmKey
    @EdmProperty(name = "docId", facets = @EdmFacets(maxLength = 255))
    private String docId;

    @EdmNavigationProperty(name = "docType", toType = CarrierRefDocumentType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private CarrierRefDocumentType docType;

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

    public CarrierRefDocumentType getDocType() {
        return docType;
    }

    public void setDocType(CarrierRefDocumentType docType) {
        this.docType = docType;
    }
}
