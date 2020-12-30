package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = InboundDeliveryItemTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class InboundDeliveryItemTP {
    public static final String ENTITY_SET_NAME = "InboundDeliveryInboundDeliveryItemTPs";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;


    @EdmNavigationProperty(name = "inboundDeliveryItem", toType = InboundDeliveryItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private InboundDeliveryItem inboundDeliveryItem;

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

    @Override
    public String toString() {
        return "InboundDeliveryItemTP{" +
                "lineNo=" + lineNo +
                ", inboundDeliveryItem=" + inboundDeliveryItem +
                '}';
    }
}
