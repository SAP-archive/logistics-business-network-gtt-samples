package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = Delivery.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class Delivery {
    public static final String ENTITY_SET_NAME = "Delivery";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "deliveryNo", facets = @EdmFacets(maxLength = 10))
    private String deliveryNo;

    @EdmProperty(name = "shipToParty", facets = @EdmFacets(maxLength = 10))
    private String shipToParty;

    @EdmProperty(name = "documentDate", type = EdmType.DATE_TIME)

    private Long documentDate;

    @EdmProperty(name = "plannedDate", type = EdmType.DATE_TIME)
    private Long plannedDate;

    @EdmProperty(name = "totalWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal totalWeight;

    @EdmProperty(name = "netWeight", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal netWeight;

    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;

    @EdmProperty(name = "volume", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal volume;

    @EdmProperty(name = "volumeUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;

    @EdmProperty(name = "pickingStatus_code")
    @SerializedName("pickingStatus_code")
    private String pickingStatusCode;

    @EdmProperty(name = "packingStatus_code")
    @SerializedName("packingStatus_code")
    private String packingStatusCode;

    @EdmProperty(name = "goodsIssueStatus_code")
    @SerializedName("goodsIssueStatus_code")
    private String goodsIssueStatusCode;

    @EdmProperty(name = "podStatus_code")
    @SerializedName("podStatus_code")
    private String podStatusCode;

    @EdmProperty(name = "executionStatus_code")
    @SerializedName("executionStatus_code")
    private String executionStatusCode;

    @EdmProperty(name = "transportationStatus_code")
    @SerializedName("transportationStatus_code")
    private String transportationStatusCode;

    @EdmProperty(name = "warehouseNo", facets = @EdmFacets(maxLength = 3))
    private String warehouseNo;

    @EdmProperty(name = "door", facets = @EdmFacets(maxLength = 3))
    private String door;

    @EdmProperty(name = "shippingPoint", facets = @EdmFacets(maxLength = 4))
    private String shippingPoint;

    @EdmProperty(name = "departureAddress", facets = @EdmFacets(maxLength = 255))
    private String departureAddress;

    @EdmProperty(name = "destination", facets = @EdmFacets(maxLength = 10))
    private String destination;

    @EdmProperty(name = "destinationAddress", facets = @EdmFacets(maxLength = 255))
    private String destinationAddress;

    @EdmProperty(name = "destinationTelephone", facets = @EdmFacets(maxLength = 30))
    private String destinationTelephone;

    @EdmProperty(name = "incoterms_code")
    @SerializedName("incoterms_code")
    private String incotermsCode;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 4))
    private String incotermsVersion;

    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;

    @EdmProperty(name = "billOfLading", facets = @EdmFacets(maxLength = 35))
    private String billOfLading;

    @EdmProperty(name = "incotermsLocation1", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation1;

    @EdmProperty(name = "warehouseDescription", facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;

    @EdmProperty(name = "departureCountry", facets = @EdmFacets(maxLength = 2))
    private String departureCountry;

    @EdmProperty(name = "destinationCountry", facets = @EdmFacets(maxLength = 2))
    private String destinationCountry;

    @EdmProperty(name = "destinationEmail", facets = @EdmFacets(maxLength = 255))
    private String destinationEmail;

    @EdmNavigationProperty(name = "pickingStatus", toType = OperationStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatus pickingStatus;

    @EdmNavigationProperty(name = "packingStatus", toType = OperationStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatus packingStatus;

    @EdmNavigationProperty(name = "goodsIssueStatus", toType = OperationStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatus goodsIssueStatus;

    @EdmNavigationProperty(name = "podStatus", toType = OperationStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatus podStatus;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "transportationStatus", toType = OperationStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private OperationStatus transportationStatus;

    @EdmNavigationProperty(name = "deliveryItemTPs", toType = DeliveryDeliveryItemTP.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<DeliveryDeliveryItemTP> deliveryItemTPs;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "shipmentTPs", toType = ShipmentTP.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<ShipmentTP> shipmentTPs;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;

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

    public String getShipToParty() {
        return shipToParty;
    }

    public void setShipToParty(String shipToParty) {
        this.shipToParty = shipToParty;
    }

    public Long getDocumentDate() {
        return documentDate;
    }

    public void setDocumentDate(Long documentDate) {
        this.documentDate = documentDate;
    }

    public Long getPlannedDate() {
        return plannedDate;
    }

    public void setPlannedDate(Long plannedDate) {
        this.plannedDate = plannedDate;
    }

    public BigDecimal getTotalWeight() {
        return totalWeight;
    }

    public void setTotalWeight(BigDecimal totalWeight) {
        this.totalWeight = totalWeight;
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

    public String getPickingStatusCode() {
        return pickingStatusCode;
    }

    public void setPickingStatusCode(String pickingStatusCode) {
        this.pickingStatusCode = pickingStatusCode;
    }

    public String getPackingStatusCode() {
        return packingStatusCode;
    }

    public void setPackingStatusCode(String packingStatusCode) {
        this.packingStatusCode = packingStatusCode;
    }

    public String getGoodsIssueStatusCode() {
        return goodsIssueStatusCode;
    }

    public void setGoodsIssueStatusCode(String goodsIssueStatusCode) {
        this.goodsIssueStatusCode = goodsIssueStatusCode;
    }

    public String getPodStatusCode() {
        return podStatusCode;
    }

    public void setPodStatusCode(String podStatusCode) {
        this.podStatusCode = podStatusCode;
    }

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public String getTransportationStatusCode() {
        return transportationStatusCode;
    }

    public void setTransportationStatusCode(String transportationStatusCode) {
        this.transportationStatusCode = transportationStatusCode;
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

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getIncotermsVersion() {
        return incotermsVersion;
    }

    public void setIncotermsVersion(String incotermsVersion) {
        this.incotermsVersion = incotermsVersion;
    }

    public String getBillOfLading() {
        return billOfLading;
    }

    public void setBillOfLading(String billOfLading) {
        this.billOfLading = billOfLading;
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

    public String getDepartureCountry() {
        return departureCountry;
    }

    public void setDepartureCountry(String departureCountry) {
        this.departureCountry = departureCountry;
    }

    public String getDestinationCountry() {
        return destinationCountry;
    }

    public void setDestinationCountry(String destinationCountry) {
        this.destinationCountry = destinationCountry;
    }

    public String getDestinationEmail() {
        return destinationEmail;
    }

    public void setDestinationEmail(String destinationEmail) {
        this.destinationEmail = destinationEmail;
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

    public ExecutionStatus getExecutionStatus() {
        return executionStatus;
    }

    public void setExecutionStatus(ExecutionStatus executionStatus) {
        this.executionStatus = executionStatus;
    }

    public OperationStatus getTransportationStatus() {
        return transportationStatus;
    }

    public void setTransportationStatus(OperationStatus transportationStatus) {
        this.transportationStatus = transportationStatus;
    }

    public List<DeliveryDeliveryItemTP> getDeliveryItemTPs() {
        return deliveryItemTPs;
    }

    public void setDeliveryItemTPs(List<DeliveryDeliveryItemTP> deliveryItemTPs) {
        this.deliveryItemTPs = deliveryItemTPs;
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

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }
}
