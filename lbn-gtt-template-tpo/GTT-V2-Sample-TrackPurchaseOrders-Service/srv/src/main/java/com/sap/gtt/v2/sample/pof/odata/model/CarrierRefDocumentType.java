package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = CarrierRefDocumentType.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class CarrierRefDocumentType {
    public static final String ENTITY_SET_NAME = "CarrierRefDocumentType";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = CarrierRefDocumentTypeTexts.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private CarrierRefDocumentTypeTexts localized;

    public CarrierRefDocumentTypeTexts getLocalized() {
        return localized;
    }

    public void setLocalized(CarrierRefDocumentTypeTexts localized) {
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
        return ENTITY_SET_NAME + "{" +
                "code='" + code + '\'' +
                ", name='" + name + '\'' +
                '}';
    }
}
