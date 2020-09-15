package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = TrackedObjectType.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class TrackedObjectType {
    public static final String ENTITY_SET_NAME = "TrackedObjectType";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = TrackedObjectTypeTexts.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TrackedObjectTypeTexts localized;

    public TrackedObjectTypeTexts getLocalized() {
        return localized;
    }

    public void setLocalized(TrackedObjectTypeTexts localized) {
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
