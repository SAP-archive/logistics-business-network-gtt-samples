package com.sap.gtt.v2.sample.pof.odata.model;

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
@EdmEntitySet(name = InboundDelivery.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class InboundDelivery {
    public static final String ENTITY_SET_NAME = "InboundDelivery";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "inboundDeliveryNo", facets = @EdmFacets(maxLength = 10))
    private String inboundDeliveryNo;

    @EdmProperty(name = "plannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long plannedDeliveryDate;

    @EdmProperty(name = "totalWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal totalWeight;

    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;

    @EdmProperty(name = "volume", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal volume;

    @EdmProperty(name = "doorForWarehouse", facets = @EdmFacets(maxLength = 3))
    private String doorForWarehouse;

    @EdmProperty(name = "doorText", facets = @EdmFacets(maxLength = 255))
    private String doorText;

    @EdmProperty(name = "warehouseNo", facets = @EdmFacets(maxLength = 3))
    private String warehouseNo;

    @EdmProperty(name = "warehouseDescription", facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;

    @EdmProperty(name = "supplierId", facets = @EdmFacets(maxLength = 255))
    private String supplierId;

    @EdmProperty(name = "documentDate", type = EdmType.DATE_TIME)
    private Long documentDate;

    @EdmProperty(name = "netWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal netWeight;

    @EdmProperty(name = "volumeUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;

    @EdmProperty(name = "billOfLading", facets = @EdmFacets(maxLength = 255))
    private String billOfLading;

    @EdmProperty(name = "dangerousGoods", type = EdmType.BOOLEAN)
    private Boolean dangerousGoods;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 255))
    private String incotermsVersion;

    @EdmProperty(name = "incotermsLocation", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @EdmProperty(name = "supplierDescription", facets = @EdmFacets(maxLength = 255))
    private String supplierDescription;

    @EdmNavigationProperty(name = "inboundDeliveryItemTPs", toType = InboundDeliveryItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<InboundDeliveryItem> inboundDeliveryItemTPs;

    @EdmNavigationProperty(name = "supplierLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType supplierLocationType;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "shipmentTPs", toType = ShipmentTP.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<ShipmentTP> shipmentTPs;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getInboundDeliveryNo() {
        return inboundDeliveryNo;
    }

    public void setInboundDeliveryNo(String inboundDeliveryNo) {
        this.inboundDeliveryNo = inboundDeliveryNo;
    }

    public Long getPlannedDeliveryDate() {
        return plannedDeliveryDate;
    }

    public void setPlannedDeliveryDate(Long plannedDeliveryDate) {
        this.plannedDeliveryDate = plannedDeliveryDate;
    }

    public BigDecimal getTotalWeight() {
        return totalWeight;
    }

    public void setTotalWeight(BigDecimal totalWeight) {
        this.totalWeight = totalWeight;
    }

    public String getWeightUoM() {
        return weightUoM;
    }

    public void setWeightUoM(String weightUoM) {
        this.weightUoM = weightUoM;
    }

    public BigDecimal getVolume() {
        return volume;
    }

    public void setVolume(BigDecimal volume) {
        this.volume = volume;
    }

    public String getDoorForWarehouse() {
        return doorForWarehouse;
    }

    public void setDoorForWarehouse(String doorForWarehouse) {
        this.doorForWarehouse = doorForWarehouse;
    }

    public String getDoorText() {
        return doorText;
    }

    public void setDoorText(String doorText) {
        this.doorText = doorText;
    }

    public String getWarehouseNo() {
        return warehouseNo;
    }

    public void setWarehouseNo(String warehouseNo) {
        this.warehouseNo = warehouseNo;
    }

    public String getWarehouseDescription() {
        return warehouseDescription;
    }

    public void setWarehouseDescription(String warehouseDescription) {
        this.warehouseDescription = warehouseDescription;
    }

    public String getSupplierId() {
        return supplierId;
    }

    public void setSupplierId(String supplierId) {
        this.supplierId = supplierId;
    }

    public Long getDocumentDate() {
        return documentDate;
    }

    public void setDocumentDate(Long documentDate) {
        this.documentDate = documentDate;
    }

    public BigDecimal getNetWeight() {
        return netWeight;
    }

    public void setNetWeight(BigDecimal netWeight) {
        this.netWeight = netWeight;
    }

    public String getVolumeUoM() {
        return volumeUoM;
    }

    public void setVolumeUoM(String volumeUoM) {
        this.volumeUoM = volumeUoM;
    }

    public String getBillOfLading() {
        return billOfLading;
    }

    public void setBillOfLading(String billOfLading) {
        this.billOfLading = billOfLading;
    }

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
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

    public String getSupplierDescription() {
        return supplierDescription;
    }

    public void setSupplierDescription(String supplierDescription) {
        this.supplierDescription = supplierDescription;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public List<InboundDeliveryItem> getInboundDeliveryItemTPs() {
        return inboundDeliveryItemTPs;
    }

    public void setInboundDeliveryItemTPs(List<InboundDeliveryItem> inboundDeliveryItemTPs) {
        this.inboundDeliveryItemTPs = inboundDeliveryItemTPs;
    }

    public LocationType getSupplierLocationType() {
        return supplierLocationType;
    }

    public void setSupplierLocationType(LocationType supplierLocationType) {
        this.supplierLocationType = supplierLocationType;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public List<ShipmentTP> getShipmentTPs() {
        return shipmentTPs;
    }

    public void setShipmentTPs(List<ShipmentTP> shipmentTPs) {
        this.shipmentTPs = shipmentTPs;
    }

    @Override
    public String toString() {
        return "InboundDelivery{" +
                "id=" + id +
                ", inboundDeliveryNo='" + inboundDeliveryNo + '\'' +
                ", plannedDeliveryDate=" + plannedDeliveryDate +
                ", totalWeight=" + totalWeight +
                ", weightUoM='" + weightUoM + '\'' +
                ", volume=" + volume +
                ", doorForWarehouse='" + doorForWarehouse + '\'' +
                ", doorText='" + doorText + '\'' +
                ", warehouseNo='" + warehouseNo + '\'' +
                ", warehouseDescription='" + warehouseDescription + '\'' +
                ", supplierId='" + supplierId + '\'' +
                ", documentDate=" + documentDate +
                ", netWeight=" + netWeight +
                ", volumeUoM='" + volumeUoM + '\'' +
                ", billOfLading='" + billOfLading + '\'' +
                ", dangerousGoods=" + dangerousGoods +
                ", incotermsVersion='" + incotermsVersion + '\'' +
                ", incotermsLocation='" + incotermsLocation + '\'' +
                ", trackingIdType='" + trackingIdType + '\'' +
                ", processStatusCode='" + processStatusCode + '\'' +
                ", supplierDescription='" + supplierDescription + '\'' +
                ", inboundDeliveryItemTPs=" + inboundDeliveryItemTPs +
                ", supplierLocationType=" + supplierLocationType +
                ", incoterms=" + incoterms +
                ", shipmentTPs=" + shipmentTPs +
                '}';
    }
}
