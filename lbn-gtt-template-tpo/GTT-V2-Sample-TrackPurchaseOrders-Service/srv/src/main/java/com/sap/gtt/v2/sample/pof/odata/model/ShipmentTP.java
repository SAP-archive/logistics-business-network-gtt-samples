package com.sap.gtt.v2.sample.pof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmFacets;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmType;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ShipmentTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ShipmentTP {
    public static final String ENTITY_SET_NAME = "InboundDeliveryShipmentTPs";


    @EdmKey
    @EdmProperty(name = "lineNo", type = EdmType.INT32)
    private Integer lineNo;

    @SerializedName("shipment_id")
    @EdmProperty(name = "shipment_id", type = EdmType.GUID)
    private UUID shipmentId;

    @EdmProperty(name = "shipmentAltKey", facets = @EdmFacets(maxLength = 255))
    private String shipmentAltKey;

    @EdmProperty(name = "firstStop", facets = @EdmFacets(maxLength = 255))
    private String firstStop;

    @EdmProperty(name = "lastStop", facets = @EdmFacets(maxLength = 255))
    private String lastStop;

    @EdmNavigationProperty(name = "shipment", toType = Shipment.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private Shipment shipment;

    @EdmNavigationProperty(name = "inboundDelivery", toType = InboundDelivery.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private InboundDelivery inboundDelivery;

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

    public String getShipmentAltKey() {
        return shipmentAltKey;
    }

    public void setShipmentAltKey(String shipmentAltKey) {
        this.shipmentAltKey = shipmentAltKey;
    }

    public String getFirstStop() {
        return firstStop;
    }

    public void setFirstStop(String firstStop) {
        this.firstStop = firstStop;
    }

    public String getLastStop() {
        return lastStop;
    }

    public void setLastStop(String lastStop) {
        this.lastStop = lastStop;
    }

    public Shipment getShipment() {
        return shipment;
    }

    public void setShipment(Shipment shipment) {
        this.shipment = shipment;
    }

    public InboundDelivery getInboundDelivery() {
        return inboundDelivery;
    }

    public void setInboundDelivery(InboundDelivery inboundDelivery) {
        this.inboundDelivery = inboundDelivery;
    }

    @Override
    public String toString() {
        return "ShipmentTP{" +
                "lineNo=" + lineNo +
                ", shipmentId=" + shipmentId +
                ", shipmentAltKey='" + shipmentAltKey + '\'' +
                ", firstStop='" + firstStop + '\'' +
                ", lastStop='" + lastStop + '\'' +
                ", shipment=" + shipment +
                ", inboundDelivery=" + inboundDelivery +
                '}';
    }
}
