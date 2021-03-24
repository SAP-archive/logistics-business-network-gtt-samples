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
@EdmEntitySet(name = InboundDeliveryItem.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class InboundDeliveryItem {
    public static final String ENTITY_SET_NAME = "InboundDeliveryItem";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "altKey")
    private String altKey;

    @EdmProperty(name = "inboundDeliveryNo", facets = @EdmFacets(maxLength = 10))
    private String inboundDeliveryNo;

    @EdmProperty(name = "itemNo", facets = @EdmFacets(maxLength = 6))
    private String itemNo;

    @EdmProperty(name = "materialNumber", facets = @EdmFacets(maxLength = 255))
    private String materialNumber;

    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 255))
    private String materialDescription;

    @EdmProperty(name = "orderQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal orderQuantity;

    @EdmProperty(name = "orderQuantityUoM", facets = @EdmFacets(maxLength = 3))
    private String orderQuantityUoM;

    @EdmProperty(name = "supplier", facets = @EdmFacets(maxLength = 255))
    private String supplier;

    @EdmProperty(name = "plannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long plannedDeliveryDate;

    @EdmProperty(name = "documentDate", type = EdmType.DATE_TIME)
    private Long documentDate;

    @EdmProperty(name = "grossWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal grossWeight;

    @EdmProperty(name = "netWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal netWeight;

    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;

    @EdmProperty(name = "volume", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal volume;

    @EdmProperty(name = "volumeUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;

    @EdmProperty(name = "warehouseNo", facets = @EdmFacets(maxLength = 3))
    private String warehouseNo;

    @EdmProperty(name = "warehouseDescription", facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;

    @EdmProperty(name = "doorForWarehouse", facets = @EdmFacets(maxLength = 3))
    private String doorForWarehouse;

    @EdmProperty(name = "itemDescription", facets = @EdmFacets(maxLength = 255))
    private String itemDescription;

    @EdmProperty(name = "doorText", facets = @EdmFacets(maxLength = 255))
    private String doorText;

    @EdmProperty(name = "billOfLading", facets = @EdmFacets(maxLength = 255))
    private String billOfLading;

    @EdmProperty(name = "dangerousGoods", type = EdmType.BOOLEAN)
    private Boolean dangerousGoods;

    @EdmProperty(name = "destination", facets = @EdmFacets(maxLength = 255))
    private String destination;

    @EdmProperty(name = "destinationAddress", facets = @EdmFacets(maxLength = 255))
    private String destinationAddress;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 255))
    private String incotermsVersion;

    @EdmProperty(name = "incotermsLocation", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation;

    @EdmProperty(name = "departure", facets = @EdmFacets(maxLength = 10))
    private String departure;

    @EdmProperty(name = "departureAddress", facets = @EdmFacets(maxLength = 255))
    private String departureAddress;

    @EdmProperty(name = "departureEmail", facets = @EdmFacets(maxLength = 255))
    private String departureEmail;

    @EdmProperty(name = "departureTelephone", facets = @EdmFacets(maxLength = 30))
    private String departureTelephone;

    @EdmProperty(name = "plant", facets = @EdmFacets(maxLength = 10))
    private String plant;

    @EdmProperty(name = "plantLocationDescription", facets = @EdmFacets(maxLength = 255))
    private String plantLocationDescription;

    @SerializedName("purchaseOrderItem_id")
    @EdmProperty(name = "purchaseOrderItem_id", type = EdmType.GUID)
    private UUID purchaseOrderItemId;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "partyId")
    private String partyId;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @SerializedName("processStatus_code")
    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @SerializedName("executionStatus_code")
    @EdmProperty(name = "executionStatus_code", facets = @EdmFacets(maxLength = 20))
    private String executionStatusCode;

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 255))
    private String incotermsCode;

    @EdmProperty(name = "plantLocationType_code")
    @SerializedName("plantLocationType_code")
    private String plantLocationTypeCode;

    @EdmProperty(name = "lastReportedEvent", facets = @EdmFacets(maxLength = 255))
    private String lastReportedEvent;

    @EdmProperty(name = "plannedArrivalTimestamp", type = EdmType.DATE_TIME_OFFSET)
    private Long plannedArrivalTimestamp;

    @EdmProperty(name = "lastEventName")
    private String lastEventName;

    @EdmProperty(name = "lastLocationAltKey")
    private String lastLocationAltKey;

    @EdmProperty(name = "lastLocationDescription")
    private String lastLocationDescription;

    @EdmProperty(name = "lastVPLocationType_code")
    @SerializedName("lastVPLocationType_code")
    private String lastVPLocationTypeCode;

    @EdmProperty(name = "revisedPlannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long revisedPlannedDeliveryDate;

    @EdmProperty(name = "initialPlannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long initialPlannedDeliveryDate;

    @EdmNavigationProperty(name = "supplierLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType supplierLocationType;

    @EdmNavigationProperty(name = "plantLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType plantLocationType;

    @EdmNavigationProperty(name = "inboundDelivery", toType = InboundDelivery.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private InboundDelivery inboundDelivery;

    @EdmNavigationProperty(name = "purchaseOrderItem", toType = PurchaseOrderItem.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private PurchaseOrderItem purchaseOrderItem;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = Constants.PLANT_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO plantLocation;

    @EdmNavigationProperty(name = Constants.SUPPLIER_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO supplierLocation;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "arrivalTimes",toType = ArrivalTime.class,toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<ArrivalTime> arrivalTimes;

    @EdmNavigationProperty(name = "lastVPLocationType", toType = VPLocationType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private VPLocationType lastVPLocationType;

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

    public String getInboundDeliveryNo() {
        return inboundDeliveryNo;
    }

    public void setInboundDeliveryNo(String inboundDeliveryNo) {
        this.inboundDeliveryNo = inboundDeliveryNo;
    }

    public String getItemNo() {
        return itemNo;
    }

    public void setItemNo(String itemNo) {
        this.itemNo = itemNo;
    }

    public String getMaterialNumber() {
        return materialNumber;
    }

    public void setMaterialNumber(String materialNumber) {
        this.materialNumber = materialNumber;
    }

    public String getMaterialDescription() {
        return materialDescription;
    }

    public void setMaterialDescription(String materialDescription) {
        this.materialDescription = materialDescription;
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

    public String getSupplier() {
        return supplier;
    }

    public void setSupplier(String supplier) {
        this.supplier = supplier;
    }

    public Long getPlannedDeliveryDate() {
        return plannedDeliveryDate;
    }

    public void setPlannedDeliveryDate(Long plannedDeliveryDate) {
        this.plannedDeliveryDate = plannedDeliveryDate;
    }

    public Long getDocumentDate() {
        return documentDate;
    }

    public void setDocumentDate(Long documentDate) {
        this.documentDate = documentDate;
    }

    public BigDecimal getGrossWeight() {
        return grossWeight;
    }

    public void setGrossWeight(BigDecimal grossWeight) {
        this.grossWeight = grossWeight;
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

    public String getDoorForWarehouse() {
        return doorForWarehouse;
    }

    public void setDoorForWarehouse(String doorForWarehouse) {
        this.doorForWarehouse = doorForWarehouse;
    }

    public String getItemDescription() {
        return itemDescription;
    }

    public void setItemDescription(String itemDescription) {
        this.itemDescription = itemDescription;
    }

    public String getDoorText() {
        return doorText;
    }

    public void setDoorText(String doorText) {
        this.doorText = doorText;
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

    public String getDestination() {
        return destination;
    }

    public void setDestination(String destination) {
        this.destination = destination;
    }

    public String getDestinationAddress() {
        return destinationAddress;
    }

    public void setDestinationAddress(String destinationAddress) {
        this.destinationAddress = destinationAddress;
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

    public String getDeparture() {
        return departure;
    }

    public void setDeparture(String departure) {
        this.departure = departure;
    }

    public String getDepartureAddress() {
        return departureAddress;
    }

    public void setDepartureAddress(String departureAddress) {
        this.departureAddress = departureAddress;
    }

    public String getDepartureEmail() {
        return departureEmail;
    }

    public void setDepartureEmail(String departureEmail) {
        this.departureEmail = departureEmail;
    }

    public String getDepartureTelephone() {
        return departureTelephone;
    }

    public void setDepartureTelephone(String departureTelephone) {
        this.departureTelephone = departureTelephone;
    }

    public String getPlant() {
        return plant;
    }

    public void setPlant(String plant) {
        this.plant = plant;
    }

    public String getPlantLocationDescription() {
        return plantLocationDescription;
    }

    public void setPlantLocationDescription(String plantLocationDescription) {
        this.plantLocationDescription = plantLocationDescription;
    }

    public UUID getPurchaseOrderItemId() {
        return purchaseOrderItemId;
    }

    public void setPurchaseOrderItemId(UUID purchaseOrderItemId) {
        this.purchaseOrderItemId = purchaseOrderItemId;
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

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getPlantLocationTypeCode() {
        return plantLocationTypeCode;
    }

    public void setPlantLocationTypeCode(String plantLocationTypeCode) {
        this.plantLocationTypeCode = plantLocationTypeCode;
    }

    public String getLastReportedEvent() {
        return lastReportedEvent;
    }

    public void setLastReportedEvent(String lastReportedEvent) {
        this.lastReportedEvent = lastReportedEvent;
    }

    public Long getPlannedArrivalTimestamp() {
        return plannedArrivalTimestamp;
    }

    public void setPlannedArrivalTimestamp(Long plannedArrivalTimestamp) {
        this.plannedArrivalTimestamp = plannedArrivalTimestamp;
    }

    public LocationType getSupplierLocationType() {
        return supplierLocationType;
    }

    public void setSupplierLocationType(LocationType supplierLocationType) {
        this.supplierLocationType = supplierLocationType;
    }

    public LocationType getPlantLocationType() {
        return plantLocationType;
    }

    public void setPlantLocationType(LocationType plantLocationType) {
        this.plantLocationType = plantLocationType;
    }

    public InboundDelivery getInboundDelivery() {
        return inboundDelivery;
    }

    public void setInboundDelivery(InboundDelivery inboundDelivery) {
        this.inboundDelivery = inboundDelivery;
    }

    public PurchaseOrderItem getPurchaseOrderItem() {
        return purchaseOrderItem;
    }

    public void setPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem) {
        this.purchaseOrderItem = purchaseOrderItem;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public LocationDTO getPlantLocation() {
        return plantLocation;
    }

    public void setPlantLocation(LocationDTO plantLocation) {
        this.plantLocation = plantLocation;
    }

    public LocationDTO getSupplierLocation() {
        return supplierLocation;
    }

    public void setSupplierLocation(LocationDTO supplierLocation) {
        this.supplierLocation = supplierLocation;
    }

    public ExecutionStatus getExecutionStatus() {
        return executionStatus;
    }

    public void setExecutionStatus(ExecutionStatus executionStatus) {
        this.executionStatus = executionStatus;
    }

    public List<ArrivalTime> getArrivalTimes() {
        return arrivalTimes;
    }

    public void setArrivalTimes(List<ArrivalTime> arrivalTimes) {
        this.arrivalTimes = arrivalTimes;
    }

    public String getLastEventName() {
        return lastEventName;
    }

    public void setLastEventName(String lastEventName) {
        this.lastEventName = lastEventName;
    }

    public String getLastLocationAltKey() {
        return lastLocationAltKey;
    }

    public void setLastLocationAltKey(String lastLocationAltKey) {
        this.lastLocationAltKey = lastLocationAltKey;
    }

    public String getLastLocationDescription() {
        return lastLocationDescription;
    }

    public void setLastLocationDescription(String lastLocationDescription) {
        this.lastLocationDescription = lastLocationDescription;
    }

    public String getLastVPLocationTypeCode() {
        return lastVPLocationTypeCode;
    }

    public void setLastVPLocationTypeCode(String lastVPLocationTypeCode) {
        this.lastVPLocationTypeCode = lastVPLocationTypeCode;
    }

    public VPLocationType getLastVPLocationType() {
        return lastVPLocationType;
    }

    public void setLastVPLocationType(VPLocationType lastVPLocationType) {
        this.lastVPLocationType = lastVPLocationType;
    }

    public Long getRevisedPlannedDeliveryDate() {
        return revisedPlannedDeliveryDate;
    }

    public void setRevisedPlannedDeliveryDate(Long revisedPlannedDeliveryDate) {
        this.revisedPlannedDeliveryDate = revisedPlannedDeliveryDate;
    }

    public Long getInitialPlannedDeliveryDate() {
        return initialPlannedDeliveryDate;
    }

    public void setInitialPlannedDeliveryDate(Long initialPlannedDeliveryDate) {
        this.initialPlannedDeliveryDate = initialPlannedDeliveryDate;
    }

    @Override
    public String toString() {
        return "InboundDeliveryItem{" +
                "id=" + id +
                ", altKey='" + altKey + '\'' +
                ", inboundDeliveryNo='" + inboundDeliveryNo + '\'' +
                ", itemNo='" + itemNo + '\'' +
                ", materialNumber='" + materialNumber + '\'' +
                ", materialDescription='" + materialDescription + '\'' +
                ", orderQuantity=" + orderQuantity +
                ", orderQuantityUoM='" + orderQuantityUoM + '\'' +
                ", supplier='" + supplier + '\'' +
                ", plannedDeliveryDate=" + plannedDeliveryDate +
                ", documentDate=" + documentDate +
                ", grossWeight=" + grossWeight +
                ", netWeight=" + netWeight +
                ", weightUoM='" + weightUoM + '\'' +
                ", volume=" + volume +
                ", volumeUoM='" + volumeUoM + '\'' +
                ", warehouseNo='" + warehouseNo + '\'' +
                ", warehouseDescription='" + warehouseDescription + '\'' +
                ", doorForWarehouse='" + doorForWarehouse + '\'' +
                ", itemDescription='" + itemDescription + '\'' +
                ", doorText='" + doorText + '\'' +
                ", billOfLading='" + billOfLading + '\'' +
                ", dangerousGoods=" + dangerousGoods +
                ", destination='" + destination + '\'' +
                ", destinationAddress='" + destinationAddress + '\'' +
                ", incotermsVersion='" + incotermsVersion + '\'' +
                ", incotermsLocation='" + incotermsLocation + '\'' +
                ", departure='" + departure + '\'' +
                ", departureAddress='" + departureAddress + '\'' +
                ", departureEmail='" + departureEmail + '\'' +
                ", departureTelephone='" + departureTelephone + '\'' +
                ", plant='" + plant + '\'' +
                ", plantLocationDescription='" + plantLocationDescription + '\'' +
                ", purchaseOrderItemId=" + purchaseOrderItemId +
                ", logicalSystem='" + logicalSystem + '\'' +
                ", partyId='" + partyId + '\'' +
                ", trackingIdType='" + trackingIdType + '\'' +
                ", processStatusCode='" + processStatusCode + '\'' +
                ", executionStatusCode='" + executionStatusCode + '\'' +
                ", incotermsCode='" + incotermsCode + '\'' +
                ", plantLocationTypeCode='" + plantLocationTypeCode + '\'' +
                ", lastReportedEvent='" + lastReportedEvent + '\'' +
                ", plannedArrivalTimestamp=" + plannedArrivalTimestamp +
                ", lastEventName='" + lastEventName + '\'' +
                ", lastLocationAltKey='" + lastLocationAltKey + '\'' +
                ", lastLocationDescription='" + lastLocationDescription + '\'' +
                ", lastVPLocationTypeCode='" + lastVPLocationTypeCode + '\'' +
                ", revisedPlannedDeliveryDate=" + revisedPlannedDeliveryDate +
                ", initialPlannedDeliveryDate=" + initialPlannedDeliveryDate +
                ", supplierLocationType=" + supplierLocationType +
                ", plantLocationType=" + plantLocationType +
                ", inboundDelivery=" + inboundDelivery +
                ", purchaseOrderItem=" + purchaseOrderItem +
                ", incoterms=" + incoterms +
                ", plantLocation=" + plantLocation +
                ", supplierLocation=" + supplierLocation +
                ", executionStatus=" + executionStatus +
                ", arrivalTimes=" + arrivalTimes +
                ", lastVPLocationType=" + lastVPLocationType +
                '}';
    }
}
