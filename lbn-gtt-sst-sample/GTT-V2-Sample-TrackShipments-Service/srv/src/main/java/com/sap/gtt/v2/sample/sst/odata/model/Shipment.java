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

/**
 * @author Aliaksandr Miron
 */
@EdmEntityType(namespace = MODEL_NAMESPACE)
@EdmEntitySet(name = Shipment.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class Shipment {

    public static final String ENTITY_SET_NAME = "Shipment";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "subaccountId")
    private UUID subaccountId;

    @EdmProperty(name = "cloneInstanceId")
    private UUID cloneInstanceId;

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

    @EdmProperty(name = "shipmentNo", facets = @EdmFacets(maxLength = 10))
    private String shipmentNo;

    @EdmProperty(name = "serviceAgentLbnId", facets = @EdmFacets(maxLength = 60))
    private String serviceAgentLbnId;

    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;

    @EdmProperty(name = "forwardingAgentTrackingId", facets = @EdmFacets(maxLength = 35))
    private String forwardingAgentTrackingId;

    @EdmProperty(name = "trackId", facets = @EdmFacets(maxLength = 255))
    private String trackId;

    @EdmProperty(name = "incotermLocation", facets = @EdmFacets(maxLength = 255))
    private String incotermLocation;

    @EdmProperty(name = "plannedTotalDistance", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal plannedTotalDistance;

    @EdmProperty(name = "plannedGrossDuration", facets = @EdmFacets(maxLength = 50))
    private String plannedGrossDuration;

    @EdmProperty(name = "plannedNetDuration", facets = @EdmFacets(maxLength = 50))
    private String plannedNetDuration;

    @EdmProperty(name = "vehicle", facets = @EdmFacets(maxLength = 40))
    private String vehicle;

    @EdmProperty(name = "registrationCountry", facets = @EdmFacets(maxLength = 3))
    private String registrationCountry;

    @EdmProperty(name = "registrationNumber", facets = @EdmFacets(maxLength = 20))
    private String registrationNumber;

    @SerializedName("cargoVolumn")
    @EdmProperty(name = "cargoVolumn", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal cargoVolume;

    @SerializedName("volumnUoM")
    @EdmProperty(name = "volumnUoM", facets = @EdmFacets(maxLength = 3))
    private String volumeUoM;

    @EdmProperty(name = "cargoWeight", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal cargoWeight;

    @EdmProperty(name = "weightUoM", facets = @EdmFacets(maxLength = 3))
    private String weightUoM;

    @EdmProperty(name = "cargoQuantity", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal cargoQuantity;

    @EdmProperty(name = "quantityUoM", facets = @EdmFacets(maxLength = 3))
    private String quantityUoM;

    @EdmProperty(name = "departureLocationId", facets = @EdmFacets(maxLength = 255))
    private String departureLocationId;

    @EdmProperty(name = "plannedDepartureDateTime", type = DATE_TIME_OFFSET)
    private Long plannedDepartureDateTime;

    @EdmProperty(name = "plannedDepartureDateTimeZone", facets = @EdmFacets(maxLength = 50))
    private String plannedDepartureDateTimeZone;

    @EdmProperty(name = "arrivalLocationId", facets = @EdmFacets(maxLength = 255))
    private String arrivalLocationId;

    @EdmProperty(name = "plannedArrivalDateTime", type = DATE_TIME_OFFSET)
    private Long plannedArrivalDateTime;

    @EdmProperty(name = "plannedArrivalDateTimeZone", facets = @EdmFacets(maxLength = 50))
    private String plannedArrivalDateTimeZone;

    @SerializedName("processStatus_code")
    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @SerializedName("transportationMode_code")
    @EdmProperty(name = "transportationMode_code", facets = @EdmFacets(maxLength = 20))
    private String transportationModeCode;

    @SerializedName("shippingType_code")
    @EdmProperty(name = "shippingType_code", facets = @EdmFacets(maxLength = 20))
    private String shippingTypeCode;

    @SerializedName("executionStatus_code")
    @EdmProperty(name = "executionStatus_code", facets = @EdmFacets(maxLength = 20))
    private String executionStatusCode;

    @SerializedName("trafficDirection_code")
    @EdmProperty(name = "trafficDirection_code", facets = @EdmFacets(maxLength = 20))
    private String trafficDirectionCode;

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 20))
    private String incotermsCode;

    @SerializedName("transportMeans_code")
    @EdmProperty(name = "transportMeans_code", facets = @EdmFacets(maxLength = 20))
    private String transportMeansCode;

    @SerializedName("departureLocationType_code")
    @EdmProperty(name = "departureLocationType_code", facets = @EdmFacets(maxLength = 20))
    private String departureLocationTypeCode;

    @SerializedName("arrivalLocationType_code")
    @EdmProperty(name = "arrivalLocationType_code", facets = @EdmFacets(maxLength = 20))
    private String arrivalLocationTypeCode;

    @EdmProperty(name = "deliveryId")
    private String deliveryId;

    @EdmNavigationProperty(name = "transportationMode", toType = TransportationMode.class, toMultiplicity = ONE)
    private TransportationMode transportationMode;

    @EdmNavigationProperty(name = "shippingType", toType = ShippingType.class, toMultiplicity = ONE)
    private ShippingType shippingType;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "trafficDirection", toType = TrafficDirection.class, toMultiplicity = ONE)
    private TrafficDirection trafficDirection;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "arrivalLocation", toType = Location.class, toMultiplicity = ONE)
    private Location arrivalLocation;

    @EdmNavigationProperty(name = "departureLocation", toType = Location.class, toMultiplicity = ONE)
    private Location departureLocation;

    @EdmNavigationProperty(name = "eventStatus", toType = EventStatus.class, toMultiplicity = ONE)
    private EventStatus eventStatus;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterm.class, toMultiplicity = ONE)
    private Incoterm incoterms;

    @EdmNavigationProperty(name = "transportMeans", toType = TransportMeansStandardCode.class, toMultiplicity = ONE)
    private TransportMeansStandardCode transportMeans;

    @EdmNavigationProperty(name = "plannedEvents", toType = PlannedEvent.class, toMultiplicity = MANY)
    private List<PlannedEvent> plannedEvents;

    @EdmNavigationProperty(name = "carrierRefDocuments", toType = CarrierReferenceDocument.class, toMultiplicity = MANY)
    private List<CarrierReferenceDocument> carrierRefDocuments;

    @EdmNavigationProperty(name = "stopsForVP", toType = StopsForVp.class, toMultiplicity = MANY)
    private List<StopsForVp> stopsForVp;

    @EdmNavigationProperty(name = "deliveryTPs", toType = DeliveryTp.class, toMultiplicity = MANY)
    private List<DeliveryTp> deliveryTps;

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

    public UUID getCloneInstanceId() {
        return cloneInstanceId;
    }

    public void setCloneInstanceId(UUID cloneInstanceId) {
        this.cloneInstanceId = cloneInstanceId;
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

    public String getTrackId() {
        return trackId;
    }

    public void setTrackId(String trackId) {
        this.trackId = trackId;
    }

    public String getIncotermLocation() {
        return incotermLocation;
    }

    public void setIncotermLocation(String incotermLocation) {
        this.incotermLocation = incotermLocation;
    }

    public BigDecimal getPlannedTotalDistance() {
        return plannedTotalDistance;
    }

    public void setPlannedTotalDistance(BigDecimal plannedTotalDistance) {
        this.plannedTotalDistance = plannedTotalDistance;
    }

    public String getPlannedGrossDuration() {
        return plannedGrossDuration;
    }

    public void setPlannedGrossDuration(String plannedGrossDuration) {
        this.plannedGrossDuration = plannedGrossDuration;
    }

    public String getPlannedNetDuration() {
        return plannedNetDuration;
    }

    public void setPlannedNetDuration(String plannedNetDuration) {
        this.plannedNetDuration = plannedNetDuration;
    }

    public String getVehicle() {
        return vehicle;
    }

    public void setVehicle(String vehicle) {
        this.vehicle = vehicle;
    }

    public String getRegistrationCountry() {
        return registrationCountry;
    }

    public void setRegistrationCountry(String registrationCountry) {
        this.registrationCountry = registrationCountry;
    }

    public String getRegistrationNumber() {
        return registrationNumber;
    }

    public void setRegistrationNumber(String registrationNumber) {
        this.registrationNumber = registrationNumber;
    }

    public BigDecimal getCargoVolume() {
        return cargoVolume;
    }

    public void setCargoVolume(BigDecimal cargoVolume) {
        this.cargoVolume = cargoVolume;
    }

    public String getVolumeUoM() {
        return volumeUoM;
    }

    public void setVolumeUoM(String volumeUoM) {
        this.volumeUoM = volumeUoM;
    }

    public BigDecimal getCargoWeight() {
        return cargoWeight;
    }

    public void setCargoWeight(BigDecimal cargoWeight) {
        this.cargoWeight = cargoWeight;
    }

    public String getWeightUoM() {
        return weightUoM;
    }

    public void setWeightUoM(String weightUoM) {
        this.weightUoM = weightUoM;
    }

    public BigDecimal getCargoQuantity() {
        return cargoQuantity;
    }

    public void setCargoQuantity(BigDecimal cargoQuantity) {
        this.cargoQuantity = cargoQuantity;
    }

    public String getQuantityUoM() {
        return quantityUoM;
    }

    public void setQuantityUoM(String quantityUoM) {
        this.quantityUoM = quantityUoM;
    }

    public String getDepartureLocationId() {
        return departureLocationId;
    }

    public void setDepartureLocationId(String departureLocationId) {
        this.departureLocationId = departureLocationId;
    }

    public Long getPlannedDepartureDateTime() {
        return plannedDepartureDateTime;
    }

    public void setPlannedDepartureDateTime(Long plannedDepartureDateTime) {
        this.plannedDepartureDateTime = plannedDepartureDateTime;
    }

    public String getPlannedDepartureDateTimeZone() {
        return plannedDepartureDateTimeZone;
    }

    public void setPlannedDepartureDateTimeZone(String plannedDepartureDateTimeZone) {
        this.plannedDepartureDateTimeZone = plannedDepartureDateTimeZone;
    }

    public String getArrivalLocationId() {
        return arrivalLocationId;
    }

    public void setArrivalLocationId(String arrivalLocationId) {
        this.arrivalLocationId = arrivalLocationId;
    }

    public Long getPlannedArrivalDateTime() {
        return plannedArrivalDateTime;
    }

    public void setPlannedArrivalDateTime(Long plannedArrivalDateTime) {
        this.plannedArrivalDateTime = plannedArrivalDateTime;
    }

    public String getPlannedArrivalDateTimeZone() {
        return plannedArrivalDateTimeZone;
    }

    public void setPlannedArrivalDateTimeZone(String plannedArrivalDateTimeZone) {
        this.plannedArrivalDateTimeZone = plannedArrivalDateTimeZone;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public String getTransportationModeCode() {
        return transportationModeCode;
    }

    public void setTransportationModeCode(String transportationModeCode) {
        this.transportationModeCode = transportationModeCode;
    }

    public String getShippingTypeCode() {
        return shippingTypeCode;
    }

    public void setShippingTypeCode(String shippingTypeCode) {
        this.shippingTypeCode = shippingTypeCode;
    }

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public String getTrafficDirectionCode() {
        return trafficDirectionCode;
    }

    public void setTrafficDirectionCode(String trafficDirectionCode) {
        this.trafficDirectionCode = trafficDirectionCode;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getTransportMeansCode() {
        return transportMeansCode;
    }

    public void setTransportMeansCode(String transportMeansCode) {
        this.transportMeansCode = transportMeansCode;
    }

    public String getDepartureLocationTypeCode() {
        return departureLocationTypeCode;
    }

    public void setDepartureLocationTypeCode(String departureLocationTypeCode) {
        this.departureLocationTypeCode = departureLocationTypeCode;
    }

    public String getArrivalLocationTypeCode() {
        return arrivalLocationTypeCode;
    }

    public void setArrivalLocationTypeCode(String arrivalLocationTypeCode) {
        this.arrivalLocationTypeCode = arrivalLocationTypeCode;
    }

    public TransportationMode getTransportationMode() {
        return transportationMode;
    }

    public void setTransportationMode(TransportationMode transportationMode) {
        this.transportationMode = transportationMode;
    }

    public ShippingType getShippingType() {
        return shippingType;
    }

    public void setShippingType(ShippingType shippingType) {
        this.shippingType = shippingType;
    }

    public ExecutionStatus getExecutionStatus() {
        return executionStatus;
    }

    public void setExecutionStatus(ExecutionStatus executionStatus) {
        this.executionStatus = executionStatus;
    }

    public TrafficDirection getTrafficDirection() {
        return trafficDirection;
    }

    public void setTrafficDirection(TrafficDirection trafficDirection) {
        this.trafficDirection = trafficDirection;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public Location getArrivalLocation() {
        return arrivalLocation;
    }

    public void setArrivalLocation(Location arrivalLocation) {
        this.arrivalLocation = arrivalLocation;
    }

    public Location getDepartureLocation() {
        return departureLocation;
    }

    public void setDepartureLocation(Location departureLocation) {
        this.departureLocation = departureLocation;
    }

    public EventStatus getEventStatus() {
        return eventStatus;
    }

    public void setEventStatus(EventStatus eventStatus) {
        this.eventStatus = eventStatus;
    }

    public Incoterm getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterm incoterms) {
        this.incoterms = incoterms;
    }

    public TransportMeansStandardCode getTransportMeans() {
        return transportMeans;
    }

    public void setTransportMeans(TransportMeansStandardCode transportMeans) {
        this.transportMeans = transportMeans;
    }

    public List<PlannedEvent> getPlannedEvents() {
        return plannedEvents;
    }

    public void setPlannedEvents(List<PlannedEvent> plannedEvents) {
        this.plannedEvents = plannedEvents;
    }

    public List<CarrierReferenceDocument> getCarrierRefDocuments() {
        return carrierRefDocuments;
    }

    public void setCarrierRefDocuments(List<CarrierReferenceDocument> carrierRefDocuments) {
        this.carrierRefDocuments = carrierRefDocuments;
    }

    public List<StopsForVp> getStopsForVp() {
        return stopsForVp;
    }

    public void setStopsForVp(List<StopsForVp> stopsForVp) {
        this.stopsForVp = stopsForVp;
    }

    public String getDeliveryId() {
        return deliveryId;
    }

    public void setDeliveryId(String deliveryId) {
        this.deliveryId = deliveryId;
    }

    public List<DeliveryTp> getDeliveryTps() {
        return deliveryTps;
    }

    public void setDeliveryTps(List<DeliveryTp> deliveryTps) {
        this.deliveryTps = deliveryTps;
    }
}
