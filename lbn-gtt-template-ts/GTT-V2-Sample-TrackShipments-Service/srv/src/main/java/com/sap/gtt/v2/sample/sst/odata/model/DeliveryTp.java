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
@EdmEntitySet(name = DeliveryTp.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class DeliveryTp {

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

    @EdmNavigationProperty(name = "delivery", toType = Delivery.class, toMultiplicity = ONE)
    private Delivery delivery;

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

    public Delivery getDelivery() {
        return delivery;
    }

    public void setDelivery(Delivery delivery) {
        this.delivery = delivery;
    }
}
