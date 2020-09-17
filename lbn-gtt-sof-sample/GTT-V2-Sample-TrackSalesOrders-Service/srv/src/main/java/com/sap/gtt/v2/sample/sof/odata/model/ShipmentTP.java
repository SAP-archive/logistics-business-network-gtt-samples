package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ShipmentTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ShipmentTP {
    public static final String ENTITY_SET_NAME = "ShipmentTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "delivery_id")
    @SerializedName("delivery_id")
    private UUID deliveryId;

    @EdmProperty(name = "shipmentAltKey", facets = @EdmFacets(maxLength = 255))
    private String shipmentAltKey;

    @EdmProperty(name = "shipment_id")
    @SerializedName("shipment_id")
    private UUID shipmentId;

    @EdmNavigationProperty(name = "shipment", toType = Shipment.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
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
