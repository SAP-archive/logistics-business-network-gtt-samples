package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = SalesOrderItemDeliveryItemTP.ENTITY_NAME)
@EdmEntitySet(name = SalesOrderItemDeliveryItemTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class SalesOrderItemDeliveryItemTP {
    public static final String ENTITY_NAME = "SalesOrderItemDeliveryItemTPs";
    public static final String ENTITY_SET_NAME = "SalesOrderItemDeliveryItemTPs";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "salesOrderItem_id")
    @SerializedName("salesOrderItem_id")
    private UUID salesOrderItemId;

    @EdmProperty(name = "deliveryItemAltKey", facets = @EdmFacets(maxLength = 255))
    private String deliveryItemAltKey;

    @EdmProperty(name = "deliveryItem_id")
    @SerializedName("deliveryItem_id")
    private UUID deliveryItemId;

    @EdmNavigationProperty(name = "deliveryItem", toType = DeliveryItem.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private DeliveryItem deliveryItem;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }


    public UUID getSalesOrderItemId() {
        return salesOrderItemId;
    }

    public void setSalesOrderItemId(UUID salesOrderItemId) {
        this.salesOrderItemId = salesOrderItemId;
    }

    public String getDeliveryItemAltKey() {
        return deliveryItemAltKey;
    }

    public void setDeliveryItemAltKey(String deliveryItemAltKey) {
        this.deliveryItemAltKey = deliveryItemAltKey;
    }

    public UUID getDeliveryItemId() {
        return deliveryItemId;
    }

    public void setDeliveryItemId(UUID deliveryItemId) {
        this.deliveryItemId = deliveryItemId;
    }

    public DeliveryItem getDeliveryItem() {
        return deliveryItem;
    }

    public void setDeliveryItem(DeliveryItem deliveryItem) {
        this.deliveryItem = deliveryItem;
    }

}
