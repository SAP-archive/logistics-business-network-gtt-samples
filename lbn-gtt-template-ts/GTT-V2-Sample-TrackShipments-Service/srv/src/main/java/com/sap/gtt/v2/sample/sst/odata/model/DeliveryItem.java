package com.sap.gtt.v2.sample.sst.odata.model;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ENTITY_CONTAINER_NAME;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.MANY;
import static org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty.Multiplicity.ONE;
import static org.apache.olingo.odata2.api.annotation.edm.EdmType.DATE_TIME;
import static org.apache.olingo.odata2.api.annotation.edm.EdmType.DATE_TIME_OFFSET;

import com.fasterxml.jackson.annotation.JsonIgnore;
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

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = DeliveryItem.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class DeliveryItem {

    public static final String ENTITY_SET_NAME = "DeliveryItem";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "partyId", facets = @EdmFacets(maxLength = 50))
    private String partyId;

    @EdmProperty(name = "altKey", facets = @EdmFacets(maxLength = 255))
    private String altKey;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @EdmProperty(name = "trackingId", facets = @EdmFacets(maxLength = 50))
    private String trackingId;

    @EdmProperty(name = "deliveryNo", facets = @EdmFacets(maxLength = 10))
    private String deliveryNo;

    @EdmProperty(name = "itemNo", facets = @EdmFacets(maxLength = 6))
    private String itemNo;

    @EdmProperty(name = "materialNo", facets = @EdmFacets(maxLength = 40))
    private String materialNo;

    @EdmProperty(name = "materialDescription", facets = @EdmFacets(maxLength = 40))
    private String materialDescription;

    @EdmProperty(name = "quantityUoM", facets = @EdmFacets(maxLength = 3))
    private String quantityUoM;

    @EdmProperty(name = "upc", facets = @EdmFacets(maxLength = 18))
    private String upc;

    @EdmProperty(name = "initialPlannedDate", type = DATE_TIME)
    private Long initialPlannedDate;

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

    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;

    @EdmProperty(name = "estimatedArrivalTime", type = DATE_TIME_OFFSET)
    private Long estimatedArrivalTime;

    @EdmProperty(name = "orderQuantity", facets = @EdmFacets(precision = 15, scale = 3))
    private BigDecimal orderQuantity;

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

    @EdmProperty(name = "billofLading", facets = @EdmFacets(maxLength = 35))
    private String billofLading;

    @EdmProperty(name = "incotermsLocation1", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation1;

    @EdmProperty(name = "warehouseDescription", facets = @EdmFacets(maxLength = 255))
    private String warehouseDescription;

    @EdmProperty(name = "destinationEmail", facets = @EdmFacets(maxLength = 255))
    private String destinationEmail;

    @EdmProperty(name = "revisedPlannedDate", type = DATE_TIME)
    private Long revisedPlannedDate;

    @EdmProperty(name = "documentDate", type = DATE_TIME)
    private Long documentDate;

    @EdmProperty(name = "lastCorrelatedEventId")
    private UUID lastCorrelatedEventId;

    @EdmProperty(name = "lastEventName", facets = @EdmFacets(maxLength = 255))
    private String lastEventName;

    @EdmProperty(name = "lastLocationAltKey", facets = @EdmFacets(maxLength = 255))
    private String lastLocationAltKey;

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

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 20))
    private String incotermsCode;

    @SerializedName("delivery_id")
    @EdmProperty(name = "delivery_id")
    private UUID deliveryId;

    @SerializedName("destinationLocationType_code")
    @EdmProperty(name = "destinationLocationType_code", facets = @EdmFacets(maxLength = 20))
    private String destinationLocationTypeCode;

    @SerializedName("lastVPLocationType_code")
    @EdmProperty(name = "lastVPLocationType_code", facets = @EdmFacets(maxLength = 20))
    private String lastVPLocationTypeCode;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterm.class, toMultiplicity = ONE)
    private Incoterm incoterms;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "pickingStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus pickingStatus;

    @EdmNavigationProperty(name = "packingStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus packingStatus;

    @EdmNavigationProperty(name = "goodsIssueStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus goodsIssueStatus;

    @EdmNavigationProperty(name = "podStatus", toType = OperationStatus.class, toMultiplicity = ONE)
    private OperationStatus podStatus;

    @EdmNavigationProperty(name = "delivery", toType = Delivery.class, toMultiplicity = ONE)
    private Delivery delivery;

    @EdmNavigationProperty(name = "plannedEvents", toType = PlannedEvent.class, toMultiplicity = MANY)
    private List<PlannedEvent> plannedEvents;

    @JsonIgnore
    private String eventStatusCode;

    @JsonIgnore
    private String eventMatchKey;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getPartyId() {
        return partyId;
    }

    public void setPartyId(String partyId) {
        this.partyId = partyId;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
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

    public String getQuantityUoM() {
        return quantityUoM;
    }

    public void setQuantityUoM(String quantityUoM) {
        this.quantityUoM = quantityUoM;
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

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
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

    public Long getRevisedPlannedDate() {
        return revisedPlannedDate;
    }

    public void setRevisedPlannedDate(Long revisedPlannedDate) {
        this.revisedPlannedDate = revisedPlannedDate;
    }

    public Long getDocumentDate() {
        return documentDate;
    }

    public void setDocumentDate(Long documentDate) {
        this.documentDate = documentDate;
    }

    public UUID getLastCorrelatedEventId() {
        return lastCorrelatedEventId;
    }

    public void setLastCorrelatedEventId(UUID lastCorrelatedEventId) {
        this.lastCorrelatedEventId = lastCorrelatedEventId;
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

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public UUID getDeliveryId() {
        return deliveryId;
    }

    public void setDeliveryId(UUID deliveryId) {
        this.deliveryId = deliveryId;
    }

    public String getDestinationLocationTypeCode() {
        return destinationLocationTypeCode;
    }

    public void setDestinationLocationTypeCode(String destinationLocationTypeCode) {
        this.destinationLocationTypeCode = destinationLocationTypeCode;
    }

    public String getLastVPLocationTypeCode() {
        return lastVPLocationTypeCode;
    }

    public void setLastVPLocationTypeCode(String lastVPLocationTypeCode) {
        this.lastVPLocationTypeCode = lastVPLocationTypeCode;
    }

    public Incoterm getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterm incoterms) {
        this.incoterms = incoterms;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
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

    public Delivery getDelivery() {
        return delivery;
    }

    public void setDelivery(Delivery delivery) {
        this.delivery = delivery;
    }

    public List<PlannedEvent> getPlannedEvents() {
        return plannedEvents;
    }

    public void setPlannedEvents(List<PlannedEvent> plannedEvents) {
        this.plannedEvents = plannedEvents;
    }

    public String getEventStatusCode() {
        return eventStatusCode;
    }

    public void setEventStatusCode(String eventStatusCode) {
        this.eventStatusCode = eventStatusCode;
    }

    public String getEventMatchKey() {
        return eventMatchKey;
    }

    public void setEventMatchKey(String eventMatchKey) {
        this.eventMatchKey = eventMatchKey;
    }
}
