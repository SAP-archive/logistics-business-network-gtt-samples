package com.sap.gtt.v2.sample.pof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = DeliveryTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class DeliveryTP {
    public static final String ENTITY_SET_NAME = "DeliveryTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "shipment_id")
    @SerializedName("shipment_id")
    private UUID shipmentId;

    @EdmProperty(name = "deliveryAltKey", facets = @EdmFacets(maxLength = 255))
    private String deliveryAltKey;

    @EdmProperty(name = "delivery_id")
    @SerializedName("delivery_id")
    private UUID deliveryId;

    @EdmNavigationProperty(name = "delivery", toType = InboundDelivery.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private InboundDelivery delivery;

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

    public String getDeliveryAltKey() {
        return deliveryAltKey;
    }

    public void setDeliveryAltKey(String deliveryAltKey) {
        this.deliveryAltKey = deliveryAltKey;
    }

    public UUID getDeliveryId() {
        return deliveryId;
    }

    public void setDeliveryId(UUID deliveryId) {
        this.deliveryId = deliveryId;
    }

    public InboundDelivery getDelivery() {
        return delivery;
    }

    public void setDelivery(InboundDelivery delivery) {
        this.delivery = delivery;
    }

}
