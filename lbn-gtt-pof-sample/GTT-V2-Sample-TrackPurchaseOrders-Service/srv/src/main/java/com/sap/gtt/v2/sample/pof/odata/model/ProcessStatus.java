package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ProcessStatus.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ProcessStatus {
    public static final String ENTITY_SET_NAME = "ProcessStatus";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = ProcessStatusTexts.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatusTexts localized;

    public ProcessStatusTexts getLocalized() {
        return localized;
    }

    public void setLocalized(ProcessStatusTexts localized) {
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
