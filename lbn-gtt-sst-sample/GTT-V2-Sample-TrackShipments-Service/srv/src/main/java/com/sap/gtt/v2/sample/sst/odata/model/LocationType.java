package com.sap.gtt.v2.sample.sst.odata.model;

import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;

import com.sap.gtt.v2.sample.sst.common.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = LocationType.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class LocationType {

    public static final String ENTITY_SET_NAME = "LocationType";

    @EdmKey
    @EdmProperty(name = "code")
    private String code;

    @EdmProperty(name = "name")
    private String name;

    @EdmNavigationProperty(name = "localized", toType = LocationTypeText.class, toMultiplicity = ONE)
    private LocationTypeText localized;

    public LocationTypeText getLocalized() {
        return localized;
    }

    public void setLocalized(LocationTypeText localized) {
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
