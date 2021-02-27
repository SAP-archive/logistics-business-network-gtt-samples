package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;

import com.google.gson.annotations.SerializedName;
import java.util.UUID;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmFacets;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = FreightUnitTp.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class FreightUnitTp {

    public static final String ENTITY_SET_NAME = "FreightUnitTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "shipment_id")
    @SerializedName("shipment_id")
    private UUID shipmentId;

    @EdmProperty(name = "freightUnit_id")
    @SerializedName("freightUnit_id")
    private UUID freightUnitId;

    @EdmProperty(name = "freightUnitAltKey", facets = @EdmFacets(maxLength = 255))
    private String freightUnitAltKey;

    @EdmNavigationProperty(name = "freightUnit", toType = FreightUnit.class, toMultiplicity = ONE)
    private FreightUnit freightUnit;

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

    public UUID getFreightUnitId() {
        return freightUnitId;
    }

    public void setFreightUnitId(UUID freightUnitId) {
        this.freightUnitId = freightUnitId;
    }

    public String getFreightUnitAltKey() {
        return freightUnitAltKey;
    }

    public void setFreightUnitAltKey(String freightUnitAltKey) {
        this.freightUnitAltKey = freightUnitAltKey;
    }

    public FreightUnit getFreightUnit() {
        return freightUnit;
    }

    public void setFreightUnit(FreightUnit freightUnit) {
        this.freightUnit = freightUnit;
    }
}
