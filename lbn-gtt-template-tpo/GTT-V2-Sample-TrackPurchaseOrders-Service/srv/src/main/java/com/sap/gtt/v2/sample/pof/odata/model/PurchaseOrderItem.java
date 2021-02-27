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

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = PurchaseOrderItem.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class PurchaseOrderItem {

    public static final String ENTITY_SET_NAME = "PurchaseOrderItem";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "altKey")
    private String altKey;

    @EdmProperty(name = "purchaseOrderNo", facets = @EdmFacets(maxLength = 10))
    private String purchaseOrderNo;

    @EdmProperty(name = "itemNo", facets = @EdmFacets(maxLength = 6))
    private String itemNo;

    @EdmProperty(name = "materialId", facets = @EdmFacets(maxLength = 255))
    private String materialId;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "partyId")
    private String partyId;

    @EdmProperty(name = "supplierId", facets = @EdmFacets(maxLength = 255))
    private String supplierId;

    @EdmProperty(name = "receivingLocationId", facets = @EdmFacets(maxLength = 255))
    private String receivingLocationId;

    @EdmProperty(name = "receivingLocationDescription", facets = @EdmFacets(maxLength = 255))
    private String receivingLocationDescription;

    @EdmProperty(name = "plannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long plannedDeliveryDate;

    @EdmProperty(name = "nextETA", type = EdmType.DATE_TIME_OFFSET)
    private Long nextETA;

    @EdmProperty(name = "orderQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal orderQuantity;

    @EdmProperty(name = "orderQuantityUoM", facets = @EdmFacets(maxLength = 3))
    private String orderQuantityUoM;

    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 255))
    private String materialDescription;

    @EdmProperty(name = "netWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal netWeight;

    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;

    @EdmProperty(name = "grossWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal grossWeight;

    @EdmProperty(name = "volume", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal volume;

    @EdmProperty(name = "volumeUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;

    @EdmProperty(name = "netValue", facets = @EdmFacets(precision = 13, scale = 2))
    private BigDecimal netValue;

    @EdmProperty(name = "currency", facets = @EdmFacets(maxLength = 3))
    private String currency;

    @EdmProperty(name = "receivingLocationType_code")
    @SerializedName("receivingLocationType_code")
    private String receivingLocationTypeCode;

    @EdmProperty(name = "completedQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal completedQuantity;

    @EdmProperty(name = "completionValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal completionValue;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 255))
    private String incotermsVersion;

    @EdmProperty(name = "incotermsLocation", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation;

    @SerializedName("processStatus_code")
    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 20))
    private String incotermsCode;

    @EdmProperty(name = "purchaseOrder_id", type = EdmType.GUID)
    private UUID purchaseOrderId;

    @EdmProperty(name = "supplierDescription", facets = @EdmFacets(maxLength = 255))
    private String supplierDescription;

    @EdmProperty(name = "lastReportedEvent", facets = @EdmFacets(maxLength = 255))
    private String lastReportedEvent;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "purchaseOrder", toType = PurchaseOrder.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private PurchaseOrder purchaseOrder;

    @EdmNavigationProperty(name = "receivingLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType receivingLocationType;

    @EdmNavigationProperty(name = "supplierLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType supplierLocationType;

    @EdmNavigationProperty(name = "inboundDeliveryItems", toType = InboundDeliveryItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<InboundDeliveryItem> inboundDeliveryItems;

    @EdmNavigationProperty(name = "scheduleLines", toType = ScheduleLine.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<ScheduleLine> scheduleLines;

    @EdmNavigationProperty(name = Constants.RECEIVING_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO receivingLocation;

    @EdmNavigationProperty(name = Constants.SUPPLIER_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO supplierLocation;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getPurchaseOrderNo() {
        return purchaseOrderNo;
    }

    public void setPurchaseOrderNo(String purchaseOrderNo) {
        this.purchaseOrderNo = purchaseOrderNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public String getMaterialId() {
        return materialId;
    }

    public void setMaterialId(String materialId) {
        this.materialId = materialId;
    }

    public String getLogicalSystem() {
        return logicalSystem;
    }

    public void setLogicalSystem(String logicalSystem) {
        this.logicalSystem = logicalSystem;
    }

    public String getPartyId() {
        return partyId;
    }

    public void setPartyId(String partyId) {
        this.partyId = partyId;
    }

    public String getSupplierId() {
        return supplierId;
    }

    public void setSupplierId(String supplierId) {
        this.supplierId = supplierId;
    }

    public String getReceivingLocationId() {
        return receivingLocationId;
    }

    public void setReceivingLocationId(String receivingLocationId) {
        this.receivingLocationId = receivingLocationId;
    }

    public String getReceivingLocationDescription() {
        return receivingLocationDescription;
    }

    public void setReceivingLocationDescription(String receivingLocationDescription) {
        this.receivingLocationDescription = receivingLocationDescription;
    }

    public Long getPlannedDeliveryDate() {
        return plannedDeliveryDate;
    }

    public void setPlannedDeliveryDate(Long plannedDeliveryDate) {
        this.plannedDeliveryDate = plannedDeliveryDate;
    }

    public Long getNextETA() {
        return nextETA;
    }

    public void setNextETA(Long nextETA) {
        this.nextETA = nextETA;
    }

    public BigDecimal getOrderQuantity() {
        return orderQuantity;
    }

    public void setOrderQuantity(BigDecimal orderQuantity) {
        this.orderQuantity = orderQuantity;
    }

    public String getOrderQuantityUoM() {
        return orderQuantityUoM;
    }

    public void setOrderQuantityUoM(String orderQuantityUoM) {
        this.orderQuantityUoM = orderQuantityUoM;
    }

    public String getMaterialDescription() {
        return materialDescription;
    }

    public void setMaterialDescription(String materialDescription) {
        this.materialDescription = materialDescription;
    }

    public BigDecimal getNetWeight() {
        return netWeight;
    }

    public void setNetWeight(BigDecimal netWeight) {
        this.netWeight = netWeight;
    }

    public String getWeightUoM() {
        return weightUoM;
    }

    public void setWeightUoM(String weightUoM) {
        this.weightUoM = weightUoM;
    }

    public BigDecimal getGrossWeight() {
        return grossWeight;
    }

    public void setGrossWeight(BigDecimal grossWeight) {
        this.grossWeight = grossWeight;
    }

    public BigDecimal getVolume() {
        return volume;
    }

    public void setVolume(BigDecimal volume) {
        this.volume = volume;
    }

    public String getVolumeUoM() {
        return volumeUoM;
    }

    public void setVolumeUoM(String volumeUoM) {
        this.volumeUoM = volumeUoM;
    }

    public BigDecimal getNetValue() {
        return netValue;
    }

    public void setNetValue(BigDecimal netValue) {
        this.netValue = netValue;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
    }

    public String getReceivingLocationTypeCode() {
        return receivingLocationTypeCode;
    }

    public void setReceivingLocationTypeCode(String receivingLocationTypeCode) {
        this.receivingLocationTypeCode = receivingLocationTypeCode;
    }

    public BigDecimal getCompletedQuantity() {
        return completedQuantity;
    }

    public void setCompletedQuantity(BigDecimal completedQuantity) {
        this.completedQuantity = completedQuantity;
    }

    public BigDecimal getCompletionValue() {
        return completionValue;
    }

    public void setCompletionValue(BigDecimal completionValue) {
        this.completionValue = completionValue;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public String getIncotermsVersion() {
        return incotermsVersion;
    }

    public void setIncotermsVersion(String incotermsVersion) {
        this.incotermsVersion = incotermsVersion;
    }

    public String getIncotermsLocation() {
        return incotermsLocation;
    }

    public void setIncotermsLocation(String incotermsLocation) {
        this.incotermsLocation = incotermsLocation;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public UUID getPurchaseOrderId() {
        return purchaseOrderId;
    }

    public void setPurchaseOrderId(UUID purchaseOrderId) {
        this.purchaseOrderId = purchaseOrderId;
    }

    public String getSupplierDescription() {
        return supplierDescription;
    }

    public void setSupplierDescription(String supplierDescription) {
        this.supplierDescription = supplierDescription;
    }

    public String getLastReportedEvent() {
        return lastReportedEvent;
    }

    public void setLastReportedEvent(String lastReportedEvent) {
        this.lastReportedEvent = lastReportedEvent;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public PurchaseOrder getPurchaseOrder() {
        return purchaseOrder;
    }

    public void setPurchaseOrder(PurchaseOrder purchaseOrder) {
        this.purchaseOrder = purchaseOrder;
    }

    public LocationType getReceivingLocationType() {
        return receivingLocationType;
    }

    public void setReceivingLocationType(LocationType receivingLocationType) {
        this.receivingLocationType = receivingLocationType;
    }

    public LocationType getSupplierLocationType() {
        return supplierLocationType;
    }

    public void setSupplierLocationType(LocationType supplierLocationType) {
        this.supplierLocationType = supplierLocationType;
    }

    public List<InboundDeliveryItem> getInboundDeliveryItems() {
        return inboundDeliveryItems;
    }

    public void setInboundDeliveryItems(List<InboundDeliveryItem> inboundDeliveryItems) {
        this.inboundDeliveryItems = inboundDeliveryItems;
    }

    public List<ScheduleLine> getScheduleLines() {
        return scheduleLines;
    }

    public void setScheduleLines(List<ScheduleLine> scheduleLines) {
        this.scheduleLines = scheduleLines;
    }

    public LocationDTO getReceivingLocation() {
        return receivingLocation;
    }

    public void setReceivingLocation(LocationDTO receivingLocation) {
        this.receivingLocation = receivingLocation;
    }

    public LocationDTO getSupplierLocation() {
        return supplierLocation;
    }

    public void setSupplierLocation(LocationDTO supplierLocation) {
        this.supplierLocation = supplierLocation;
    }

    @Override
    public String toString() {
        return "PurchaseOrderItem{" +
                "id=" + id +
                ", altKey='" + altKey + '\'' +
                ", purchaseOrderNo='" + purchaseOrderNo + '\'' +
                ", itemNo='" + itemNo + '\'' +
                ", materialId='" + materialId + '\'' +
                ", logicalSystem='" + logicalSystem + '\'' +
                ", partyId='" + partyId + '\'' +
                ", supplierId='" + supplierId + '\'' +
                ", receivingLocationId='" + receivingLocationId + '\'' +
                ", receivingLocationDescription='" + receivingLocationDescription + '\'' +
                ", plannedDeliveryDate=" + plannedDeliveryDate +
                ", nextETA=" + nextETA +
                ", orderQuantity=" + orderQuantity +
                ", orderQuantityUoM='" + orderQuantityUoM + '\'' +
                ", materialDescription='" + materialDescription + '\'' +
                ", netWeight=" + netWeight +
                ", weightUoM='" + weightUoM + '\'' +
                ", grossWeight=" + grossWeight +
                ", volume=" + volume +
                ", volumeUoM='" + volumeUoM + '\'' +
                ", netValue=" + netValue +
                ", currency='" + currency + '\'' +
                ", receivingLocationTypeCode='" + receivingLocationTypeCode + '\'' +
                ", completedQuantity=" + completedQuantity +
                ", completionValue=" + completionValue +
                ", trackingIdType='" + trackingIdType + '\'' +
                ", incotermsVersion='" + incotermsVersion + '\'' +
                ", incotermsLocation='" + incotermsLocation + '\'' +
                ", processStatusCode='" + processStatusCode + '\'' +
                ", incotermsCode='" + incotermsCode + '\'' +
                ", purchaseOrderId=" + purchaseOrderId +
                ", supplierDescription='" + supplierDescription + '\'' +
                ", lastReportedEvent='" + lastReportedEvent + '\'' +
                ", processStatus=" + processStatus +
                ", incoterms=" + incoterms +
                ", purchaseOrder=" + purchaseOrder +
                ", receivingLocationType=" + receivingLocationType +
                ", supplierLocationType=" + supplierLocationType +
                ", inboundDeliveryItems=" + inboundDeliveryItems +
                ", scheduleLines=" + scheduleLines +
                ", receivingLocation=" + receivingLocation +
                ", supplierLocation=" + supplierLocation +
                '}';
    }
}
