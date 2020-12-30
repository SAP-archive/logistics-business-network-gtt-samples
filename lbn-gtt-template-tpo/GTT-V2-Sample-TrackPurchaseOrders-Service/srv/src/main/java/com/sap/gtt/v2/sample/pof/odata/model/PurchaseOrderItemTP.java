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
@EdmEntitySet(name = PurchaseOrderItemTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class PurchaseOrderItemTP {

    public static final String ENTITY_SET_NAME = "PurchaseOrderPurchaseOrderItemTPs";

    @EdmKey
    @EdmProperty(name = "lineNo", type = EdmType.INT32)
    private Integer lineNo;

    @EdmKey
    @SerializedName("purchaseOrder_id")
    @EdmProperty(name = "purchaseOrder_id", type = EdmType.GUID)
    private UUID purchaseOrderId;

    @SerializedName("purchaseOrderItem_id")
    @EdmProperty(name = "purchaseOrderItem_id", type = EdmType.GUID)
    private UUID purchaseOrderItemId;

    @EdmProperty(name = "purchaseOrderItemAltKey", facets = @EdmFacets(maxLength = 255))
    private String purchaseOrderItemAltKey;

    @EdmNavigationProperty(name = "purchaseOrder", toType = PurchaseOrder.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private PurchaseOrder purchaseOrder;

    @EdmNavigationProperty(name = "purchaseOrderItem", toType = PurchaseOrderItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private PurchaseOrderItem purchaseOrderItem;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public UUID getPurchaseOrderId() {
        return purchaseOrderId;
    }

    public void setPurchaseOrderId(UUID purchaseOrderId) {
        this.purchaseOrderId = purchaseOrderId;
    }

    public UUID getPurchaseOrderItemId() {
        return purchaseOrderItemId;
    }

    public void setPurchaseOrderItemId(UUID purchaseOrderItemId) {
        this.purchaseOrderItemId = purchaseOrderItemId;
    }

    public String getPurchaseOrderItemAltKey() {
        return purchaseOrderItemAltKey;
    }

    public void setPurchaseOrderItemAltKey(String purchaseOrderItemAltKey) {
        this.purchaseOrderItemAltKey = purchaseOrderItemAltKey;
    }

    public PurchaseOrder getPurchaseOrder() {
        return purchaseOrder;
    }

    public void setPurchaseOrder(PurchaseOrder purchaseOrder) {
        this.purchaseOrder = purchaseOrder;
    }

    public PurchaseOrderItem getPurchaseOrderItem() {
        return purchaseOrderItem;
    }

    public void setPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem) {
        this.purchaseOrderItem = purchaseOrderItem;
    }

    @Override
    public String toString() {
        return "PurchaseOrderPurchaseOrderItems{" +
                "lineNo=" + lineNo +
                ", purchaseOrderId=" + purchaseOrderId +
                ", purchaseOrderItemId=" + purchaseOrderItemId +
                ", purchaseOrderItemAltKey='" + purchaseOrderItemAltKey + '\'' +
                ", purchaseOrder=" + purchaseOrder +
                ", purchaseOrderItem=" + purchaseOrderItem +
                '}';
    }
}
