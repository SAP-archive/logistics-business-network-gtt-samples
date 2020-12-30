package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ReferenceDocument.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ReferenceDocument {
    public static final String ENTITY_SET_NAME = "ReferenceDocument";

    @EdmKey
    @EdmProperty(name = "documentType", facets = @EdmFacets(maxLength = 10))
    private String documentType;

    @EdmKey
    @EdmProperty(name = "documentNumber", facets = @EdmFacets(maxLength = 16))
    private String documentNumber;

    public String getDocumentType() {
        return documentType;
    }

    public void setDocumentType(String documentType) {
        this.documentType = documentType;
    }

    public String getDocumentNumber() {
        return documentNumber;
    }

    public void setDocumentNumber(String documentNumber) {
        this.documentNumber = documentNumber;
    }
}
