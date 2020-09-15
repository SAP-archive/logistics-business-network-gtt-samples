package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity;

@EdmEntitySet(name = DeliveryItem.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
public class DeliveryItem {
    public static final String ENTITY_SET_NAME = "DeliveryItem";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "warehouseNo" , facets = @EdmFacets(maxLength = 3))
    private String warehouseNo;
    @EdmProperty(name = "door",facets = @EdmFacets(maxLength = 3))
    private String door;
    @EdmProperty(name = "shippingPoint",facets = @EdmFacets(maxLength = 4))
    private String shippingPoint;
    @EdmProperty(name = "departureAddress",facets = @EdmFacets(maxLength = 255))
    private String departureAddress;
    @EdmProperty(name = "destination",facets = @EdmFacets(maxLength = 10))
    private String destination;
    @EdmProperty(name = "destinationAddress",facets = @EdmFacets(maxLength = 255))
    private String destinationAddress;
    @EdmProperty(name = "destinationTelephone",facets = @EdmFacets(maxLength = 30))
    private String destinationTelephone;
    @EdmProperty(name = "incotermsVersion",facets = @EdmFacets(maxLength = 4))
    private String incotermsVersion;
    @EdmProperty(name = "billofLading",facets = @EdmFacets(maxLength = 35))
    private String billofLading;
    @EdmProperty(name = "incotermsLocation1",facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation1;
    @EdmProperty(name = "warehouseDescription",facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;
    @EdmProperty(name = "destinationEmail",facets = @EdmFacets(maxLength = 255))
    private String destinationEmail;
    @EdmNavigationProperty(name = "incoterms" ,toType = Incoterms.class, toMultiplicity = Multiplicity.ONE)
    private Incoterms incoterms;


    @EdmProperty(name = "deliveryNo", facets = @EdmFacets(maxLength = 10))
    private String deliveryNo;
    @EdmProperty(name = "itemNo", facets = @EdmFacets(maxLength = 6))
    private String itemNo;
    @EdmProperty(name = "materialNo", facets = @EdmFacets(maxLength = 40))
    private String materialNo;
    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 40))
    private String materialDescription;
    @EdmProperty(name = "upc", facets = @EdmFacets(maxLength = 18))
    private String upc;
    @EdmProperty(name = "initialPlannedDate", type = EdmType.DATE_TIME)
    private Long initialPlannedDate;
    @EdmProperty(name = "grossWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal grossWeight;
    @EdmProperty(name = "netWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal netWeight;
    @EdmProperty(name = "volume", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal volume;
    @EdmProperty(name = "quantityUoM", facets = @EdmFacets(maxLength = 3))
    private String quantityUoM;
    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;
    @EdmProperty(name = "volumeUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;
    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;
    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = Multiplicity.ONE)
    private ProcessStatus processStatus;
    @EdmProperty(name = "revisedPlannedDate", type = EdmType.DATE_TIME)
    private Long revisedPlannedDate;
    @EdmProperty(name = "estimatedArrivalTime", type = EdmType.DATE_TIME_OFFSET)
    private Long estimatedArrivalTime;
    @EdmProperty(name = "orderQuantity", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal orderQuantity;
    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = Multiplicity.ONE)
    private ExecutionStatus executionStatus;
    @EdmNavigationProperty(name = "pickingStatus", toType = OperationStatus.class, toMultiplicity = Multiplicity.ONE)
    private OperationStatus pickingStatus;
    @EdmNavigationProperty(name = "packingStatus", toType = OperationStatus.class, toMultiplicity = Multiplicity.ONE)
    private OperationStatus packingStatus;
    @EdmNavigationProperty(name = "goodsIssueStatus", toType = OperationStatus.class, toMultiplicity = Multiplicity.ONE)
    private OperationStatus goodsIssueStatus;
    @EdmNavigationProperty(name = "podStatus", toType = OperationStatus.class, toMultiplicity = Multiplicity.ONE)
    private OperationStatus podStatus;
    @EdmNavigationProperty(name = "salesOrderItem", toType = SalesOrderItem.class, toMultiplicity = Multiplicity.ONE)
    private SalesOrderItem salesOrderItem;
    @EdmNavigationProperty(name = "delivery", toType = Delivery.class, toMultiplicity = Multiplicity.ONE)
    private Delivery delivery;

    @EdmProperty(name = "podStatus_code")
    @SerializedName("podStatus_code")
    private String podStatusCode;
    @EdmProperty(name="incoterms_code")
    @SerializedName("incoterms_code")
    private String incotermsCode;

    @EdmProperty(name = "salesOrderItem_id")
    @SerializedName("salesOrderItem_id")
    private UUID salesOrderItemId;
    @EdmProperty(name = "executionStatus_code")
    @SerializedName("executionStatus_code")
    private String executionStatusCode;
    @EdmProperty(name = "goodsIssueStatus_code")
    @SerializedName("goodsIssueStatus_code")
    private String goodsIssueStatusCode;
    @EdmProperty(name = "packingStatus_code")
    @SerializedName("packingStatus_code")
    private String packingStatusCode;
    @EdmProperty(name = "pickingStatus_code")
    @SerializedName("pickingStatus_code")
    private String pickingStatusCode;
    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;
    @EdmProperty(name = "documentDate", type = EdmType.DATE_TIME)
    private Long documentDate;
    @EdmProperty(name = "progress",facets = @EdmFacets(precision = 5, scale = 4))
    private BigDecimal progress;
    @EdmProperty(name = "plannedArrivalTimestamp",type = EdmType.DATE_TIME_OFFSET)
    private Long plannedArrivalTimestamp;
    @EdmProperty(name = "actualArrivalTimestamp" , type = EdmType.DATE_TIME_OFFSET)
    private Long actualArrivalTimestamp;
    @EdmProperty(name = "trackingId", facets = @EdmFacets(maxLength = 50))
    private String trackingId;

    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;
    @EdmProperty(name = "partyId" )
    private String partyId;
    @EdmProperty(name = "logicalSystem")
    private String logicalSystem;
    @EdmProperty(name = "destinationLocationType_code")
    @SerializedName("destinationLocationType_code")
    private String destinationLocationTypeCode;
    @EdmNavigationProperty(name = "arrivalTimes",toType = ArrivalTime.class,toMultiplicity = Multiplicity.MANY)
    private List<ArrivalTime> arrivalTimes;
    @EdmProperty(name = "destinationAltKey")
    private String destinationAltKey;
    @EdmNavigationProperty(name = "destinationLocation", toType = LocationDTO.class,toMultiplicity = Multiplicity.ONE)
    private LocationDTO destinationLocation;

    @EdmProperty(name = "lastEventName")
    private String lastEventName;

    @EdmProperty(name = "lastLocationAltKey")
    private String lastLocationAltKey;

    @EdmProperty(name = "lastLocationDescription")
    private String lastLocationDescription;

    @EdmProperty(name = "lastVPLocationType_code")
    @SerializedName("lastVPLocationType_code")
    private String lastVPLocationTypeCode;

    @EdmNavigationProperty(name = "lastVPLocationType", toType = VPLocationType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private VPLocationType lastVPLocationType;

    public VPLocationType getLastVPLocationType() {
        return lastVPLocationType;
    }

    public void setLastVPLocationType(VPLocationType lastVPLocationType) {
        this.lastVPLocationType = lastVPLocationType;
    }

    public void setDestinationAltKey(String destinationAltKey) {
        this.destinationAltKey = destinationAltKey;
    }

    public LocationDTO getDestinationLocation() {
        return destinationLocation;
    }

    public void setDestinationLocation(LocationDTO destinationLocation) {
        this.destinationLocation = destinationLocation;
    }

    public List<ArrivalTime> getArrivalTimes() {
        return arrivalTimes;
    }

    public void setArrivalTimes(List<ArrivalTime> arrivalTimes) {
        this.arrivalTimes = arrivalTimes;
    }

    public String getDestinationLocationTypeCode() {
        return destinationLocationTypeCode;
    }

    public void setDestinationLocationTypeCode(String destinationLocationTypeCode) {
        this.destinationLocationTypeCode = destinationLocationTypeCode;
    }

    public String getPartyId() {
        return partyId;
    }

    public void setPartyId(String partyId) {
        this.partyId = partyId;
    }

    public String getLogicalSystem() {
        return logicalSystem;
    }

    public void setLogicalSystem(String logicalSystem) {
        this.logicalSystem = logicalSystem;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public BigDecimal getProgress() {
        return progress;
    }

    public void setProgress(BigDecimal progress) {
        this.progress = progress;
    }

    public Long getPlannedArrivalTimestamp() {
        return plannedArrivalTimestamp;
    }

    public void setPlannedArrivalTimestamp(Long plannedArrivalTimestamp) {
        this.plannedArrivalTimestamp = plannedArrivalTimestamp;
    }

    public Long getActualArrivalTimestamp() {
        return actualArrivalTimestamp;
    }

    public void setActualArrivalTimestamp(Long actualArrivalTimestamp) {
        this.actualArrivalTimestamp = actualArrivalTimestamp;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public String getPodStatusCode() {
        return podStatusCode;
    }

    public void setPodStatusCode(String podStatusCode) {
        this.podStatusCode = podStatusCode;
    }

    public UUID getSalesOrderItemId() {
        return salesOrderItemId;
    }

    public void setSalesOrderItemId(UUID salesOrderItemId) {
        this.salesOrderItemId = salesOrderItemId;
    }

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public String getGoodsIssueStatusCode() {
        return goodsIssueStatusCode;
    }

    public void setGoodsIssueStatusCode(String goodsIssueStatusCode) {
        this.goodsIssueStatusCode = goodsIssueStatusCode;
    }

    public String getPackingStatusCode() {
        return packingStatusCode;
    }

    public void setPackingStatusCode(String packingStatusCode) {
        this.packingStatusCode = packingStatusCode;
    }

    public String getPickingStatusCode() {
        return pickingStatusCode;
    }

    public void setPickingStatusCode(String pickingStatusCode) {
        this.pickingStatusCode = pickingStatusCode;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getDeliveryNo() {
        return deliveryNo;
    }

    public void setDeliveryNo(String deliveryNo) {
        this.deliveryNo = deliveryNo;
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

    public String getUpc() {
        return upc;
    }

    public void setUpc(String upc) {
        this.upc = upc;
    }

    public Long getInitialPlannedDate() {
        return initialPlannedDate;
    }

    public void setInitialPlannedDate(Long initialPlannedDate) {
        this.initialPlannedDate = initialPlannedDate;
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

    public BigDecimal getVolume() {
        return volume;
    }

    public void setVolume(BigDecimal volume) {
        this.volume = volume;
    }

    public String getQuantityUoM() {
        return quantityUoM;
    }

    public void setQuantityUoM(String quantityUoM) {
        this.quantityUoM = quantityUoM;
    }

    public String getWeightUoM() {
        return weightUoM;
    }

    public void setWeightUoM(String weightUoM) {
        this.weightUoM = weightUoM;
    }

    public String getVolumeUoM() {
        return volumeUoM;
    }

    public void setVolumeUoM(String volumeUoM) {
        this.volumeUoM = volumeUoM;
    }

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }


    public Long getRevisedPlannedDate() {
        return revisedPlannedDate;
    }

    public void setRevisedPlannedDate(Long revisedPlannedDate) {
        this.revisedPlannedDate = revisedPlannedDate;
    }

    public Long getEstimatedArrivalTime() {
        return estimatedArrivalTime;
    }

    public void setEstimatedArrivalTime(Long estimatedArrivalTime) {
        this.estimatedArrivalTime = estimatedArrivalTime;
    }

    public BigDecimal getOrderQuantity() {
        return orderQuantity;
    }

    public void setOrderQuantity(BigDecimal orderQuantity) {
        this.orderQuantity = orderQuantity;
    }

    public ExecutionStatus getExecutionStatus() {
        return executionStatus;
    }

    public void setExecutionStatus(ExecutionStatus executionStatus) {
        this.executionStatus = executionStatus;
    }

    public OperationStatus getPickingStatus() {
        return pickingStatus;
    }

    public void setPickingStatus(OperationStatus pickingStatus) {
        this.pickingStatus = pickingStatus;
    }

    public OperationStatus getPackingStatus() {
        return packingStatus;
    }

    public void setPackingStatus(OperationStatus packingStatus) {
        this.packingStatus = packingStatus;
    }

    public OperationStatus getGoodsIssueStatus() {
        return goodsIssueStatus;
    }

    public void setGoodsIssueStatus(OperationStatus goodsIssueStatus) {
        this.goodsIssueStatus = goodsIssueStatus;
    }

    public OperationStatus getPodStatus() {
        return podStatus;
    }

    public void setPodStatus(OperationStatus podStatus) {
        this.podStatus = podStatus;
    }

    public SalesOrderItem getSalesOrderItem() {
        return salesOrderItem;
    }

    public void setSalesOrderItem(SalesOrderItem salesOrderItem) {
        this.salesOrderItem = salesOrderItem;
    }

    public Delivery getDelivery() {
        return delivery;
    }

    public void setDelivery(Delivery delivery) {
        this.delivery = delivery;
    }

    public String getWarehouseNo() {
        return warehouseNo;
    }

    public void setWarehouseNo(String warehouseNo) {
        this.warehouseNo = warehouseNo;
    }

    public String getDoor() {
        return door;
    }

    public void setDoor(String door) {
        this.door = door;
    }

    public String getShippingPoint() {
        return shippingPoint;
    }

    public void setShippingPoint(String shippingPoint) {
        this.shippingPoint = shippingPoint;
    }

    public String getDepartureAddress() {
        return departureAddress;
    }

    public void setDepartureAddress(String departureAddress) {
        this.departureAddress = departureAddress;
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

    public String getDestinationTelephone() {
        return destinationTelephone;
    }

    public void setDestinationTelephone(String destinationTelephone) {
        this.destinationTelephone = destinationTelephone;
    }

    public String getIncotermsVersion() {
        return incotermsVersion;
    }

    public void setIncotermsVersion(String incotermsVersion) {
        this.incotermsVersion = incotermsVersion;
    }

    public String getBillofLading() {
        return billofLading;
    }

    public void setBillofLading(String billofLading) {
        this.billofLading = billofLading;
    }

    public String getIncotermsLocation1() {
        return incotermsLocation1;
    }

    public void setIncotermsLocation1(String incotermsLocation1) {
        this.incotermsLocation1 = incotermsLocation1;
    }

    public String getWarehouseDescription() {
        return warehouseDescription;
    }

    public void setWarehouseDescription(String warehouseDescription) {
        this.warehouseDescription = warehouseDescription;
    }

    public String getDestinationEmail() {
        return destinationEmail;
    }

    public void setDestinationEmail(String destinationEmail) {
        this.destinationEmail = destinationEmail;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public Long getDocumentDate() {
        return documentDate;
    }

    public void setDocumentDate(Long documentDate) {
        this.documentDate = documentDate;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public String getDestinationAltKey() {
        return destinationAltKey;
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

    public String getLastVPLocationTypeCode() {
        return lastVPLocationTypeCode;
    }

    public void setLastVPLocationTypeCode(String lastVPLocationTypeCode) {
        this.lastVPLocationTypeCode = lastVPLocationTypeCode;
    }

    public String getLastLocationDescription() {
        return lastLocationDescription;
    }

    public void setLastLocationDescription(String lastLocationDescription) {
        this.lastLocationDescription = lastLocationDescription;
    }
}
