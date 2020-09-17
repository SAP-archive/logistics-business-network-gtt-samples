package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = Shipment.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class Shipment {
    public static final String ENTITY_SET_NAME = "Shipment";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "shipmentNo", facets = @EdmFacets(maxLength = 10))
    private String shipmentNo;

    @EdmProperty(name = "serviceAgentLbnId", facets = @EdmFacets(maxLength = 60))
    private String serviceAgentLbnId;

    @EdmProperty(name = "transportationMode_code")
    @SerializedName("transportationMode_code")
    private String transportationModeCode;

    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;

    @EdmProperty(name = "forwardingAgentTrackingId", facets = @EdmFacets(maxLength = 35))
    private String forwardingAgentTrackingId;

    @EdmProperty(name = "shippingType_code")
    @SerializedName("shippingType_code")
    private String shippingTypeCode;

    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;

    @EdmProperty(name = "trackedResourceType_code")
    @SerializedName("trackedResourceType_code")
    private String trackedResourceTypeCode;

    @EdmProperty(name = "trackedResourceId", facets = @EdmFacets(maxLength = 20))
    private String trackedResourceId;

    @EdmProperty(name = "trackID", facets = @EdmFacets(maxLength = 255))
    private String trackID;
    @EdmProperty(name = "partyId")
    private String partyId;
    @EdmProperty(name = "logicalSystem")
    private String logicalSystem;

    @EdmNavigationProperty(name = "transportationMode", toType = TransportationMode.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TransportationMode transportationMode;

    @EdmNavigationProperty(name = "stops", toType = Stop.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<Stop> stops;
    @EdmNavigationProperty(name = "stopsForVP", toType = StopsForVP.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<StopsForVP> stopsForVP;
    @EdmNavigationProperty(name = "shippingType", toType = ShippingType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ShippingType shippingType;

    @EdmNavigationProperty(name = "referenceDocuments", toType = ReferenceDocument.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<ReferenceDocument> referenceDocuments;

    @EdmNavigationProperty(name = "resourceTPs", toType = ResourceTP.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<ResourceTP> resourceTPs;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "trackedResourceType", toType = TrackedResourceType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TrackedResourceType trackedResourceType;

    @EdmNavigationProperty(name = "deliveryTPs", toType = DeliveryTP.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<DeliveryTP> deliveryTPs;

    @EdmNavigationProperty(name = "carrierRefDocuments", toType = CarrierRefDocument.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<CarrierRefDocument> carrierRefDocuments;

    @EdmNavigationProperty(name = "trackedObjects", toType = TrackedObject.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<TrackedObject> trackedObjects;
    
    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;
    @EdmProperty(name = "executionStatus_code")
    @SerializedName("executionStatus_code")
    private String executionStatusCode;

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

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getShipmentNo() {
        return shipmentNo;
    }

    public void setShipmentNo(String shipmentNo) {
        this.shipmentNo = shipmentNo;
    }

    public String getServiceAgentLbnId() {
        return serviceAgentLbnId;
    }

    public void setServiceAgentLbnId(String serviceAgentLbnId) {
        this.serviceAgentLbnId = serviceAgentLbnId;
    }

    public String getTransportationModeCode() {
        return transportationModeCode;
    }

    public void setTransportationModeCode(String transportationModeCode) {
        this.transportationModeCode = transportationModeCode;
    }

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
    }

    public String getShippingTypeCode() {
        return shippingTypeCode;
    }

    public void setShippingTypeCode(String shippingTypeCode) {
        this.shippingTypeCode = shippingTypeCode;
    }

    public String getForwardingAgentTrackingId() {
        return forwardingAgentTrackingId;
    }

    public void setForwardingAgentTrackingId(String forwardingAgentTrackingId) {
        this.forwardingAgentTrackingId = forwardingAgentTrackingId;
    }

    public TransportationMode getTransportationMode() {
        return transportationMode;
    }

    public void setTransportationMode(TransportationMode transportationMode) {
        this.transportationMode = transportationMode;
    }

    public List<Stop> getStops() {
        return stops;
    }

    public void setStops(List<Stop> stops) {
        this.stops = stops;
    }

    public List<StopsForVP> getStopsForVP() {
        return stopsForVP;
    }

    public void setStopsForVP(List<StopsForVP> stopsForVP) {
        this.stopsForVP = stopsForVP;
    }

    public ShippingType getShippingType() {
        return shippingType;
    }

    public void setShippingType(ShippingType shippingType) {
        this.shippingType = shippingType;
    }

    public List<ReferenceDocument> getReferenceDocuments() {
        return referenceDocuments;
    }

    public void setReferenceDocuments(List<ReferenceDocument> referenceDocuments) {
        this.referenceDocuments = referenceDocuments;
    }

    public List<ResourceTP> getResourceTPs() {
        return resourceTPs;
    }

    public void setResourceTPs(List<ResourceTP> resourceTPs) {
        this.resourceTPs = resourceTPs;
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

    public String getTrackedResourceTypeCode() {
        return trackedResourceTypeCode;
    }

    public void setTrackedResourceTypeCode(String trackedResourceTypeCode) {
        this.trackedResourceTypeCode = trackedResourceTypeCode;
    }

    public String getTrackedResourceId() {
        return trackedResourceId;
    }

    public void setTrackedResourceId(String trackedResourceId) {
        this.trackedResourceId = trackedResourceId;
    }

    public String getTrackID() {
        return trackID;
    }

    public void setTrackID(String trackID) {
        this.trackID = trackID;
    }

    public TrackedResourceType getTrackedResourceType() {
        return trackedResourceType;
    }

    public void setTrackedResourceType(TrackedResourceType trackedResourceType) {
        this.trackedResourceType = trackedResourceType;
    }

    public List<DeliveryTP> getDeliveryTPs() {
        return deliveryTPs;
    }

    public void setDeliveryTPs(List<DeliveryTP> deliveryTPs) {
        this.deliveryTPs = deliveryTPs;
    }

    public List<CarrierRefDocument> getCarrierRefDocuments() {
        return carrierRefDocuments;
    }

    public void setCarrierRefDocuments(List<CarrierRefDocument> carrierRefDocuments) {
        this.carrierRefDocuments = carrierRefDocuments;
    }

    public List<TrackedObject> getTrackedObjects() {
        return trackedObjects;
    }

    public void setTrackedObjects(List<TrackedObject> trackedObjects) {
        this.trackedObjects = trackedObjects;
    }

}
