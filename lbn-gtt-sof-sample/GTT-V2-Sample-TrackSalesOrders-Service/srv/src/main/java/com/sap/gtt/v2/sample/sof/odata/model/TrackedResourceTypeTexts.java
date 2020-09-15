package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;


@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = TrackedResourceTypeTexts.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class TrackedResourceTypeTexts {
    public static final String ENTITY_SET_NAME = "TrackedResourceType_texts";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmKey
    @EdmProperty(name = "locale")
    private String locale;

    @EdmProperty(name = "name")
    private String name;

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getLocale() {
        return locale;
    }

    public void setLocale(String locale) {
        this.locale = locale;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
