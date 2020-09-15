package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = Resource.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class Resource {
    public static final String ENTITY_SET_NAME = "Resource";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "resourceNo", facets = @EdmFacets(maxLength = 10))
    private String resourceNo;

    @EdmProperty(name = "serviceAgentLbnId", facets = @EdmFacets(maxLength = 60))
    private String serviceAgentLbnId;

    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;

    @EdmProperty(name = "forwardingAgentTrackingId", facets = @EdmFacets(maxLength = 35))
    private String forwardingAgentTrackingId;

    @EdmProperty(name = "trackedResourceId", facets = @EdmFacets(maxLength = 20))
    private String trackedResourceId;

    @EdmProperty(name = "trackingId", facets = @EdmFacets(maxLength = 50))
    private String trackingId;

    @EdmProperty(name = "transportationMode_code")
    @SerializedName("transportationMode_code")
    private String transportationModeCode;

    @EdmProperty(name = "incoterms_code")
    @SerializedName("incoterms_code")
    private String incotermsCode;

    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "transportationMode", toType = TransportationMode.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TransportationMode transportationMode;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "stops", toType = Stop.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<Stop> stops;

    @EdmNavigationProperty(name = "shippingType", toType = ShippingType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ShippingType shippingType;

    @EdmNavigationProperty(name = "referenceDocuments", toType = ReferenceDocument.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.MANY)
    private List<ReferenceDocument> referenceDocuments;

    @EdmNavigationProperty(name = "trackedResourceType", toType = TrackedResourceType.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private TrackedResourceType trackedResourceType;

    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getResourceNo() {
        return resourceNo;
    }

    public void setResourceNo(String resourceNo) {
        this.resourceNo = resourceNo;
    }

    public String getServiceAgentLbnId() {
        return serviceAgentLbnId;
    }

    public void setServiceAgentLbnId(String serviceAgentLbnId) {
        this.serviceAgentLbnId = serviceAgentLbnId;
    }

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
    }

    public String getForwardingAgentTrackingId() {
        return forwardingAgentTrackingId;
    }

    public void setForwardingAgentTrackingId(String forwardingAgentTrackingId) {
        this.forwardingAgentTrackingId = forwardingAgentTrackingId;
    }

    public String getTrackedResourceId() {
        return trackedResourceId;
    }

    public void setTrackedResourceId(String trackedResourceId) {
        this.trackedResourceId = trackedResourceId;
    }

    public String getTrackingId() {
        return trackingId;
    }

    public void setTrackingId(String trackingId) {
        this.trackingId = trackingId;
    }

    public String getTransportationModeCode() {
        return transportationModeCode;
    }

    public void setTransportationModeCode(String transportationModeCode) {
        this.transportationModeCode = transportationModeCode;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
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

    public TransportationMode getTransportationMode() {
        return transportationMode;
    }

    public void setTransportationMode(TransportationMode transportationMode) {
        this.transportationMode = transportationMode;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public List<Stop> getStops() {
        return stops;
    }

    public void setStops(List<Stop> stops) {
        this.stops = stops;
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

    public TrackedResourceType getTrackedResourceType() {
        return trackedResourceType;
    }

    public void setTrackedResourceType(TrackedResourceType trackedResourceType) {
        this.trackedResourceType = trackedResourceType;
    }
    
}
