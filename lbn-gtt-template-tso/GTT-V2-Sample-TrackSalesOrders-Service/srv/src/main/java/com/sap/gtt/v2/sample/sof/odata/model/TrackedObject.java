package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = TrackedObject.ENTITY_NAME)
@EdmEntitySet(name = TrackedObject.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class TrackedObject {
    public static final String ENTITY_NAME = "ShipmentTrackedObjects";
    public static final String ENTITY_SET_NAME = "ShipmentTrackedObjects";

    @EdmKey
    @EdmProperty(name = "idType_code", facets = @EdmFacets(maxLength = 20))
    @SerializedName("idType_code")
    private String idTypeCode;

    @EdmKey
    @EdmProperty(name = "value", facets = @EdmFacets(maxLength = 255))
    private String value;

    @EdmNavigationProperty(name = "idType", toType = TrackedObjectType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TrackedObjectType idType;

    public String getIdTypeCode() {
        return idTypeCode;
    }

    public void setIdTypeCode(String idTypeCode) {
        this.idTypeCode = idTypeCode;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public TrackedObjectType getIdType() {
        return idType;
    }

    public void setIdType(TrackedObjectType idType) {
        this.idType = idType;
    }

}
