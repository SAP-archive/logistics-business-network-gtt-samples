package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.gson.annotations.SerializedName;
import java.math.BigDecimal;
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
@EdmEntitySet(name = FreightUnitItem.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class FreightUnitItem {

    public static final String ENTITY_SET_NAME = "FreightUnitItem";

    @EdmKey
    @EdmProperty(name = "itemNo")
    private String itemNo;

    @EdmKey
    @EdmProperty(name = "freightUnit_id")
    @SerializedName("freightUnit_id")
    private UUID freightUnitId;

    @EdmProperty(name = "deliveryNo", facets = @EdmFacets(maxLength = 35))
    private String deliveryNo;

    @EdmProperty(name = "deliveryItemNo", facets = @EdmFacets(maxLength = 10))
    private String deliveryItemNo;

    @EdmProperty(name = "quantity", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal quantity;

    @EdmProperty(name = "quantityUoM", facets = @EdmFacets(maxLength = 3))
    private String quantityUoM;

    @EdmProperty(name = "materialNo", facets = @EdmFacets(maxLength = 40))
    private String materialNo;

    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 40))
    private String materialDescription;

    @EdmNavigationProperty(name = "deliveryItem", toType = DeliveryItem.class, toMultiplicity = ONE)
    private DeliveryItem deliveryItem;

    @JsonIgnore
    private String eventStatusCode;

    @JsonIgnore
    private String freightUnitAltKey;

    @JsonIgnore
    private String eventMatchKey;

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public UUID getFreightUnitId() {
        return freightUnitId;
    }

    public void setFreightUnitId(UUID freightUnitId) {
        this.freightUnitId = freightUnitId;
    }

    public String getDeliveryNo() {
        return deliveryNo;
    }

    public void setDeliveryNo(String deliveryNo) {
        this.deliveryNo = deliveryNo;
    }

    public String getDeliveryItemNo() {
        return deliveryItemNo;
    }

    public void setDeliveryItemNo(String deliveryItemNo) {
        this.deliveryItemNo = deliveryItemNo;
    }

    public BigDecimal getQuantity() {
        return quantity;
    }

    public void setQuantity(BigDecimal quantity) {
        this.quantity = quantity;
    }

    public String getQuantityUoM() {
        return quantityUoM;
    }

    public void setQuantityUoM(String quantityUoM) {
        this.quantityUoM = quantityUoM;
    }

    public String getMaterialNo() {
        return materialNo;
    }

    public void setMaterialNo(String materialNo) {
        this.materialNo = materialNo;
    }

    public String getMaterialDescription() {
        return materialDescription;
    }

    public void setMaterialDescription(String materialDescription) {
        this.materialDescription = materialDescription;
    }

    public DeliveryItem getDeliveryItem() {
        return deliveryItem;
    }

    public void setDeliveryItem(DeliveryItem deliveryItem) {
        this.deliveryItem = deliveryItem;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public String getFreightUnitAltKey() {
        return freightUnitAltKey;
    }

    public void setFreightUnitAltKey(String freightUnitAltKey) {
        this.freightUnitAltKey = freightUnitAltKey;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }
}
