package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = PurchaseOrderItemInboundDeliveryItemTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class PurchaseOrderItemInboundDeliveryItemTP {
    public static final String ENTITY_SET_NAME = "PurchaseOrderItemInboundDeliveryItems";

    @EdmKey
    @EdmProperty(name = "lineNo", type = EdmType.INT32)
    private Integer lineNo;

    @EdmNavigationProperty(name = "inboundDeliveryItem", toType = InboundDeliveryItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private InboundDeliveryItem inboundDeliveryItem;

    @EdmNavigationProperty(name = "purchaseOrderItem", toType = PurchaseOrderItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private PurchaseOrderItem purchaseOrderItem;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public InboundDeliveryItem getInboundDeliveryItem() {
        return inboundDeliveryItem;
    }

    public void setInboundDeliveryItem(InboundDeliveryItem inboundDeliveryItem) {
        this.inboundDeliveryItem = inboundDeliveryItem;
    }

    public PurchaseOrderItem getPurchaseOrderItem() {
        return purchaseOrderItem;
    }

    public void setPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem) {
        this.purchaseOrderItem = purchaseOrderItem;
    }

    @Override
    public String toString() {
        return "PurchaseOrderItemInboundDeliveryItemTP{" +
                "lineNo=" + lineNo +
                ", inboundDeliveryItem=" + inboundDeliveryItem +
                ", purchaseOrderItem=" + purchaseOrderItem +
                '}';
    }
}
