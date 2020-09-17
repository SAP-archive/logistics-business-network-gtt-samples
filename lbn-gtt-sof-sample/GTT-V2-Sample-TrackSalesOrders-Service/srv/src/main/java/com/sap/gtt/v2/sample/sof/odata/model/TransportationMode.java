package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = TransportationMode.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class TransportationMode {
    public static final String ENTITY_SET_NAME = "TransportationMode";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = TransportationModeTexts.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TransportationModeTexts localized;

    public TransportationModeTexts getLocalized() {
        return localized;
    }

    public void setLocalized(TransportationModeTexts localized) {
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

}
