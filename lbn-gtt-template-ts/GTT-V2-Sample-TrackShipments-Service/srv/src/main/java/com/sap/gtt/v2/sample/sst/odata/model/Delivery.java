package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.MANY;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmType.DATE_TIME_OFFSET;

import com.google.gson.annotations.SerializedName;
import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntitySet;
import org.apache.olingo.odata2.api.annotation.edm.EdmEntityType;
import org.apache.olingo.odata2.api.annotation.edm.EdmFacets;
import org.apache.olingo.odata2.api.annotation.edm.EdmKey;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmProperty;
import org.apache.olingo.odata2.api.annotation.edm.EdmType;

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = Delivery.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class Delivery {

    public static final String ENTITY_SET_NAME = "Delivery";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "subaccountId")
    private UUID subaccountId;

    @EdmProperty(name = "trackedProcessType", facets = @EdmFacets(maxLength = 255))
    private String trackedProcessType;

    @EdmProperty(name = "altKey", facets = @EdmFacets(maxLength = 255))
    private String altKey;

    @EdmProperty(name = "scheme", facets = @EdmFacets(maxLength = 255))
    private String scheme;

    @EdmProperty(name = "partyId", facets = @EdmFacets(maxLength = 50))
    private String partyId;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @EdmProperty(name = "trackingId", facets = @EdmFacets(maxLength = 50))
    private String trackingId;

    @EdmProperty(name = "lastChangedAtBusinessTime", type = DATE_TIME_OFFSET)
    private Long lastChangedAtBusinessTime;

    @EdmProperty(name = "createdByUser", facets = @EdmFacets(maxLength = 64))
    private String createdByUser;

    @EdmProperty(name = "creationDateTime", type = DATE_TIME_OFFSET)
    private Long creationDateTime;

    @EdmProperty(name = "lastChangedByUser", facets = @EdmFacets(maxLength = 64))
    private String lastChangedByUser;

    @EdmProperty(name = "lastChangeDateTime", type = DATE_TIME_OFFSET)
    private Long lastChangeDateTime;

    @EdmProperty(name = "version")
    private Integer version;

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

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 4))
    private String incotermsVersion;

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

    @SerializedName("processStatus_code")
    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @SerializedName("pickingStatus_code")
    @EdmProperty(name = "pickingStatus_code", facets = @EdmFacets(maxLength = 20))
    private String pickingStatusCode;

    @SerializedName("packingStatus_code")
    @EdmProperty(name = "packingStatus_code", facets = @EdmFacets(maxLength = 20))
    private String packingStatusCode;

    @SerializedName("goodsIssueStatus_code")
    @EdmProperty(name = "goodsIssueStatus_code", facets = @EdmFacets(maxLength = 20))
    private String goodsIssueStatusCode;

    @SerializedName("podStatus_code")
    @EdmProperty(name = "podStatus_code", facets = @EdmFacets(maxLength = 20))
    private String podStatusCode;

    @SerializedName("executionStatus_code")
    @EdmProperty(name = "executionStatus_code", facets = @EdmFacets(maxLength = 20))
    private String executionStatusCode;

    @SerializedName("transportationStatus_code")
    @EdmProperty(name = "transportationStatus_code", facets = @EdmFacets(maxLength = 20))
    private String transportationStatusCode;

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 20))
    private String incotermsCode;

    @EdmNavigationProperty(name = "pickingStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus pickingStatus;

    @EdmNavigationProperty(name = "packingStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus packingStatus;

    @EdmNavigationProperty(name = "goodsIssueStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus goodsIssueStatus;

    @EdmNavigationProperty(name = "podStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus podStatus;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "transportationStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus transportationStatus;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterm.class, toMultiplicity = ONE)
    private Incoterm incoterms;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "shipmentTPs", toType = ShipmentTp.class, toMultiplicity = MANY)
    private List<ShipmentTp> shipmentTps;

    @EdmNavigationProperty(name = "deliveryItems", toType = DeliveryItem.class, toMultiplicity = MANY)
    private List<DeliveryItem> deliveryItems;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public UUID getSubaccountId() {
        return subaccountId;
    }

    public void setSubaccountId(UUID subaccountId) {
        this.subaccountId = subaccountId;
    }

    public String getTrackedProcessType() {
        return trackedProcessType;
    }

    public void setTrackedProcessType(String trackedProcessType) {
        this.trackedProcessType = trackedProcessType;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public String getScheme() {
        return scheme;
    }

    public void setScheme(String scheme) {
        this.scheme = scheme;
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

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public Long getLastChangedAtBusinessTime() {
        return lastChangedAtBusinessTime;
    }

    public void setLastChangedAtBusinessTime(Long lastChangedAtBusinessTime) {
        this.lastChangedAtBusinessTime = lastChangedAtBusinessTime;
    }

    public String getCreatedByUser() {
        return createdByUser;
    }

    public void setCreatedByUser(String createdByUser) {
        this.createdByUser = createdByUser;
    }

    public Long getCreationDateTime() {
        return creationDateTime;
    }

    public void setCreationDateTime(Long creationDateTime) {
        this.creationDateTime = creationDateTime;
    }

    public String getLastChangedByUser() {
        return lastChangedByUser;
    }

    public void setLastChangedByUser(String lastChangedByUser) {
        this.lastChangedByUser = lastChangedByUser;
    }

    public Long getLastChangeDateTime() {
        return lastChangeDateTime;
    }

    public void setLastChangeDateTime(Long lastChangeDateTime) {
        this.lastChangeDateTime = lastChangeDateTime;
    }

    public Integer getVersion() {
        return version;
    }

    public void setVersion(Integer version) {
        this.version = version;
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

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
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

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
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

    public Incoterm getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterm incoterms) {
        this.incoterms = incoterms;
    }

    public List<ShipmentTp> getShipmentTps() {
        return shipmentTps;
    }

    public void setShipmentTps(List<ShipmentTp> shipmentTps) {
        this.shipmentTps = shipmentTps;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public List<DeliveryItem> getDeliveryItems() {
        return deliveryItems;
    }

    public void setDeliveryItems(List<DeliveryItem> deliveryItems) {
        this.deliveryItems = deliveryItems;
    }
}
