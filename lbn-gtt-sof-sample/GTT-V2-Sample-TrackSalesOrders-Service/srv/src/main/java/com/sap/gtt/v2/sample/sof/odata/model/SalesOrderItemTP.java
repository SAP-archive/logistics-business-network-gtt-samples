package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = SalesOrderItemTP.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class SalesOrderItemTP {
    public static final String ENTITY_SET_NAME = "SalesOrderItemTP";

    @EdmKey
    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmKey
    @EdmProperty(name = "salesOrder_id")
    @SerializedName("salesOrder_id")
    private UUID salesOrderId;

    @EdmProperty(name = "salesOrderItemAltKey", facets = @EdmFacets(maxLength = 255))
    private String salesOrderItemAltKey;

    @EdmProperty(name = "salesOrderItem_id")
    @SerializedName("salesOrderItem_id")
    private UUID salesOrderItemId;

    @EdmNavigationProperty(name = "salesOrderItem", toType = SalesOrderItem.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private SalesOrderItem salesOrderItem;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public UUID getSalesOrderId() {
        return salesOrderId;
    }

    public void setSalesOrderId(UUID salesOrderId) {
        this.salesOrderId = salesOrderId;
    }

    public String getSalesOrderItemAltKey() {
        return salesOrderItemAltKey;
    }

    public void setSalesOrderItemAltKey(String salesOrderItemAltKey) {
        this.salesOrderItemAltKey = salesOrderItemAltKey;
    }

    public UUID getSalesOrderItemId() {
        return salesOrderItemId;
    }

    public void setSalesOrderItemId(UUID salesOrderItemId) {
        this.salesOrderItemId = salesOrderItemId;
    }

    public SalesOrderItem getSalesOrderItem() {
        return salesOrderItem;
    }

    public void setSalesOrderItem(SalesOrderItem salesOrderItem) {
        this.salesOrderItem = salesOrderItem;
    }

}
