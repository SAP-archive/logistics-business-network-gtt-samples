package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = SalesOrderItem.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class SalesOrderItem {
    public static final String ENTITY_SET_NAME = "SalesOrderItem";
    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "salesOrderNo", facets = @EdmFacets(maxLength = 10))
    private String salesOrderNo;
    @EdmProperty(name = "itemNo", facets = @EdmFacets(maxLength = 6))
    private String itemNo;
    @EdmProperty(name = "materialNo", facets = @EdmFacets(maxLength = 40))
    private String materialNo;
    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 40))
    private String materialDescription;
    @EdmProperty(name = "plant", facets = @EdmFacets(maxLength = 4))
    private String plant;
    @EdmProperty(name = "orderQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal orderQuantity;
    @EdmProperty(name = "uom", facets = @EdmFacets(maxLength = 3))
    private String uom;
    @EdmProperty(name = "netValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal netValue;
    @EdmProperty(name = "delayedQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal delayedQuantity;
    @EdmProperty(name = "completionQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal completionQuantity;
    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;
    @EdmProperty(name = "processStatus_code")
    @SerializedName(("processStatus_code"))
    private String processStatusCode;
    @EdmProperty(name = "currency", facets = @EdmFacets(maxLength = 5))
    private String currency;

    @EdmNavigationProperty(name = "deliveryItems", toType = DeliveryItem.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<DeliveryItem> deliveryItems;

    @EdmProperty(name = "altKey")
    private String altKey;
    @EdmProperty(name = "salesOrder_id")
    @SerializedName("salesOrder_id")
    private String salesOrderId;

    @EdmProperty(name = "completedAndLateQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal completedAndLateQuantity;

    @EdmProperty(name = "isCompleted")
    private Boolean isCompleted;

    @EdmNavigationProperty(name = "salesOrder", toType = SalesOrder.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private SalesOrder salesOrder;

    @EdmProperty(name = "isDelayed")
    private Boolean isDelayed;

    @EdmProperty(name = "rejectionStatus_code", facets = @EdmFacets(maxLength = 20))
    private String rejectionStatusCode;

    @EdmNavigationProperty(name = "scheduleLines", toType = ScheduleLine.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<ScheduleLine> scheduleLines;

    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public Boolean getDelayed() {
        return isDelayed;
    }

    public void setDelayed(Boolean delayed) {
        isDelayed = delayed;
    }

    public BigDecimal getCompletedAndLateQuantity() {
        return completedAndLateQuantity;
    }

    public void setCompletedAndLateQuantity(BigDecimal completedAndLateQuantity) {
        this.completedAndLateQuantity = completedAndLateQuantity;
    }

    public Boolean getCompleted() {
        return isCompleted;
    }

    public void setCompleted(Boolean completed) {
        isCompleted = completed;
    }

    public String getSalesOrderId() {
        return salesOrderId;
    }

    public void setSalesOrderId(String salesOrderId) {
        this.salesOrderId = salesOrderId;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public List<DeliveryItem> getDeliveryItems() {
        return deliveryItems;
    }

    public void setDeliveryItems(List<DeliveryItem> deliveryItems) {
        this.deliveryItems = deliveryItems;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getSalesOrderNo() {
        return salesOrderNo;
    }

    public void setSalesOrderNo(String salesOrderNo) {
        this.salesOrderNo = salesOrderNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
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

    public String getPlant() {
        return plant;
    }

    public void setPlant(String plant) {
        this.plant = plant;
    }

    public BigDecimal getOrderQuantity() {
        return orderQuantity;
    }

    public void setOrderQuantity(BigDecimal orderQuantity) {
        this.orderQuantity = orderQuantity;
    }

    public String getUom() {
        return uom;
    }

    public void setUom(String uom) {
        this.uom = uom;
    }

    public BigDecimal getNetValue() {
        return netValue;
    }

    public void setNetValue(BigDecimal netValue) {
        this.netValue = netValue;
    }

    public BigDecimal getDelayedQuantity() {
        return delayedQuantity;
    }

    public void setDelayedQuantity(BigDecimal delayedQuantity) {
        this.delayedQuantity = delayedQuantity;
    }

    public BigDecimal getCompletionQuantity() {
        return completionQuantity;
    }

    public void setCompletionQuantity(BigDecimal completionQuantity) {
        this.completionQuantity = completionQuantity;
    }

    public SalesOrder getSalesOrder() {
        return salesOrder;
    }

    public void setSalesOrder(SalesOrder salesOrder) {
        this.salesOrder = salesOrder;
    }

    public List<ScheduleLine> getScheduleLines() {
        return scheduleLines;
    }

    public void setScheduleLines(List<ScheduleLine> scheduleLines) {
        this.scheduleLines = scheduleLines;
    }

    public String getRejectionStatusCode() {
        return rejectionStatusCode;
    }

    public void setRejectionStatusCode(String rejectionStatusCode) {
        this.rejectionStatusCode = rejectionStatusCode;
    }
}
