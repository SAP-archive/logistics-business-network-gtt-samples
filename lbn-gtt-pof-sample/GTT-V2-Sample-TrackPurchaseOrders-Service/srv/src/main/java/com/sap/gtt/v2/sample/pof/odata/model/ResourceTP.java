package com.sap.gtt.v2.sample.pof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ResourceTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ResourceTP {
    public static final String ENTITY_SET_NAME = "ResourceTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "shipment_id")
    @SerializedName("shipment_id")
    private UUID shipmentId;

    @EdmProperty(name = "resourceAltKey", facets = @EdmFacets(maxLength = 255))
    private String resourceAltKey;

    @EdmProperty(name = "resource_id")
    @SerializedName("resource_id")
    private UUID resourceId;

    @EdmNavigationProperty(name = "resource", toType = Resource.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private Resource resource;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public UUID getShipmentId() {
        return shipmentId;
    }

    public void setShipmentId(UUID shipmentId) {
        this.shipmentId = shipmentId;
    }

    public String getResourceAltKey() {
        return resourceAltKey;
    }

    public void setResourceAltKey(String resourceAltKey) {
        this.resourceAltKey = resourceAltKey;
    }

    public UUID getResourceId() {
        return resourceId;
    }

    public void setResourceId(UUID resourceId) {
        this.resourceId = resourceId;
    }

    public Resource getResource() {
        return resource;
    }

    public void setResource(Resource resource) {
        this.resource = resource;
    }
}
