package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = OperationStatus.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class OperationStatus {
    public static final String ENTITY_SET_NAME = "OperationStatus";
    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = OperationStatusTexts.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatusTexts localized;

    public OperationStatusTexts getLocalized() {
        return localized;
    }

    public void setLocalized(OperationStatusTexts localized) {
        this.localized = localized;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "OperationStatus{" +
                "code='" + code + '\'' +
                ", name='" + name + '\'' +
                '}';
    }
}
