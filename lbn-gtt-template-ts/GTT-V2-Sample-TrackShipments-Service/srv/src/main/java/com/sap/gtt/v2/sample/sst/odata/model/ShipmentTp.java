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
@EdmEntitySet(name = ShipmentTp.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class ShipmentTp {

    public static final String ENTITY_SET_NAME = "ShipmentTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @SerializedName("delivery_id")
    @EdmProperty(name = "delivery_id")
    private UUID deliveryId;

    @EdmProperty(name = "shipmentAltKey", facets = @EdmFacets(maxLength = 255))
    private String shipmentAltKey;

    @SerializedName("shipment_id")
    @EdmProperty(name = "shipment_id")
    private UUID shipmentId;

    @EdmNavigationProperty(name = "shipment", toType = Shipment.class, toMultiplicity = ONE)
    private Shipment shipment;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public UUID getDeliveryId() {
        return deliveryId;
    }

    public void setDeliveryId(UUID deliveryId) {
        this.deliveryId = deliveryId;
    }

    public String getShipmentAltKey() {
        return shipmentAltKey;
    }

    public void setShipmentAltKey(String shipmentAltKey) {
        this.shipmentAltKey = shipmentAltKey;
    }

    public UUID getShipmentId() {
        return shipmentId;
    }

    public void setShipmentId(UUID shipmentId) {
        this.shipmentId = shipmentId;
    }

    public Shipment getShipment() {
        return shipment;
    }

    public void setShipment(Shipment shipment) {
        this.shipment = shipment;
    }
}
