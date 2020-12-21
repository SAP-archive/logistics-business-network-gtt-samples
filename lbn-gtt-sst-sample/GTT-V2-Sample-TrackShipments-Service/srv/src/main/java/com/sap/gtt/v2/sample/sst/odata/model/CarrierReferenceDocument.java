package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;

import com.google.gson.annotations.SerializedName;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = CarrierReferenceDocument.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class CarrierReferenceDocument {

    public static final String ENTITY_SET_NAME = "CarrierRefDocuments";

    @EdmKey
    @EdmProperty(name = "docType_code")
    @SerializedName("docType_code")
    private String docTypeCode;

    @EdmKey
    @EdmProperty(name = "docId")
    private String docId;

    @EdmNavigationProperty(name = "docType", toType = CarrierReferenceDocumentType.class, toMultiplicity = ONE)
    private CarrierReferenceDocumentType docType;

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

    public CarrierReferenceDocumentType getDocType() {
        return docType;
    }

    public void setDocType(CarrierReferenceDocumentType docType) {
        this.docType = docType;
    }
}
