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
@EdmEntitySet(name = FreightUnit.ENTITY_SET_NAME, container = ENTITY_CONTAINER_NAME)
public class FreightUnit {

    public static final String ENTITY_SET_NAME = "FreightUnit";

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

    @EdmProperty(name = "freightUnitNo", facets = @EdmFacets(maxLength = 10))
    private String freightUnitNo;

    @EdmProperty(name = "plannedTotalDistance", facets = @EdmFacets(precision = 31, scale = 14))
    private BigDecimal plannedTotalDistance;

    @EdmProperty(name = "plannedGrossDuration", facets = @EdmFacets(maxLength = 50))
    private String plannedGrossDuration;

    @EdmProperty(name = "plannedNetDuration", facets = @EdmFacets(maxLength = 50))
    private String plannedNetDuration;

    @EdmProperty(name = "dangerousGoods")
    private Boolean dangerousGoods;

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

    @EdmProperty(name = "serviceAgentLbnId", facets = @EdmFacets(maxLength = 60))
    private String serviceAgentLbnId;

    @SerializedName("processStatus_code")
    @EdmProperty(name = "processStatus_code", facets = @EdmFacets(maxLength = 50))
    private String processStatusCode;

    @SerializedName("shippingType_code")
    @EdmProperty(name = "shippingType_code", facets = @EdmFacets(maxLength = 255))
    private String shippingTypeCode;

    @SerializedName("incoterms_code")
    @EdmProperty(name = "incoterms_code", facets = @EdmFacets(maxLength = 255))
    private String incotermsCode;

    @SerializedName("transportationMode_code")
    @EdmProperty(name = "transportationMode_code", facets = @EdmFacets(maxLength = 255))
    private String transportationModeCode;

    @SerializedName("departureLocationType_code")
    @EdmProperty(name = "departureLocationType_code", facets = @EdmFacets(maxLength = 255))
    private String departureLocationTypeCode;

    @SerializedName("arrivalLocationType_code")
    @EdmProperty(name = "arrivalLocationType_code", facets = @EdmFacets(maxLength = 255))
    private String arrivalLocationTypeCode;

    @SerializedName("executionStatus_code")
    @EdmProperty(name = "executionStatus_code", facets = @EdmFacets(maxLength = 20))
    private String executionStatusCode;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "shippingType", toType = ShippingType.class, toMultiplicity = ONE)
    private ShippingType shippingType;

    @EdmNavigationProperty(name = "departureLocationType", toType = LocationType.class, toMultiplicity = ONE)
    private LocationType departureLocationType;

    @EdmNavigationProperty(name = "arrivalLocationType", toType = LocationType.class, toMultiplicity = ONE)
    private LocationType arrivalLocationType;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterm.class, toMultiplicity = ONE)
    private Incoterm incoterms;

    @EdmNavigationProperty(name = "transportationMode", toType = TransportationMode.class, toMultiplicity = ONE)
    private TransportationMode transportationMode;

    @EdmNavigationProperty(name = "arrivalLocation", toType = Location.class, toMultiplicity = ONE)
    private Location arrivalLocation;

    @EdmNavigationProperty(name = "departureLocation", toType = Location.class, toMultiplicity = ONE)
    private Location departureLocation;

    @EdmNavigationProperty(name = "executionStatus", toType = ExecutionStatus.class, toMultiplicity = ONE)
    private ExecutionStatus executionStatus;

    @EdmNavigationProperty(name = "plannedEvents", toType = PlannedEvent.class, toMultiplicity = MANY)
    private List<PlannedEvent> plannedEvents;

    @EdmNavigationProperty(name = "carrierRefDocuments", toType = CarrierReferenceDocument.class, toMultiplicity = MANY)
    private List<CarrierReferenceDocument> carrierRefDocuments;

    @EdmNavigationProperty(name = "stopsForVP", toType = StopsForVp.class, toMultiplicity = MANY)
    private List<StopsForVp> stopsForVp;

    @EdmNavigationProperty(name = "freightUnitItems", toType = FreightUnitItem.class, toMultiplicity = MANY)
    private List<FreightUnitItem> freightUnitItems;

    @EdmNavigationProperty(name = "shipmentTPs", toType = ShipmentTp.class, toMultiplicity = MANY)
    private List<ShipmentTp> shipmentTps;

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

    public String getFreightUnitNo() {
        return freightUnitNo;
    }

    public void setFreightUnitNo(String freightUnitNo) {
        this.freightUnitNo = freightUnitNo;
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

    public Boolean getDangerousGoods() {
        return dangerousGoods;
    }

    public void setDangerousGoods(Boolean dangerousGoods) {
        this.dangerousGoods = dangerousGoods;
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

    public String getServiceAgentLbnId() {
        return serviceAgentLbnId;
    }

    public void setServiceAgentLbnId(String serviceAgentLbnId) {
        this.serviceAgentLbnId = serviceAgentLbnId;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public String getShippingTypeCode() {
        return shippingTypeCode;
    }

    public void setShippingTypeCode(String shippingTypeCode) {
        this.shippingTypeCode = shippingTypeCode;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getTransportationModeCode() {
        return transportationModeCode;
    }

    public void setTransportationModeCode(String transportationModeCode) {
        this.transportationModeCode = transportationModeCode;
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

    public String getExecutionStatusCode() {
        return executionStatusCode;
    }

    public void setExecutionStatusCode(String executionStatusCode) {
        this.executionStatusCode = executionStatusCode;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public ShippingType getShippingType() {
        return shippingType;
    }

    public void setShippingType(ShippingType shippingType) {
        this.shippingType = shippingType;
    }

    public LocationType getDepartureLocationType() {
        return departureLocationType;
    }

    public void setDepartureLocationType(LocationType departureLocationType) {
        this.departureLocationType = departureLocationType;
    }

    public LocationType getArrivalLocationType() {
        return arrivalLocationType;
    }

    public void setArrivalLocationType(LocationType arrivalLocationType) {
        this.arrivalLocationType = arrivalLocationType;
    }

    public Incoterm getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterm incoterms) {
        this.incoterms = incoterms;
    }

    public TransportationMode getTransportationMode() {
        return transportationMode;
    }

    public void setTransportationMode(TransportationMode transportationMode) {
        this.transportationMode = transportationMode;
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

    public ExecutionStatus getExecutionStatus() {
        return executionStatus;
    }

    public void setExecutionStatus(ExecutionStatus executionStatus) {
        this.executionStatus = executionStatus;
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

    public List<FreightUnitItem> getFreightUnitItems() {
        return freightUnitItems;
    }

    public void setFreightUnitItems(List<FreightUnitItem> freightUnitItems) {
        this.freightUnitItems = freightUnitItems;
    }

    public List<ShipmentTp> getShipmentTps() {
        return shipmentTps;
    }

    public void setShipmentTps(List<ShipmentTp> shipmentTps) {
        this.shipmentTps = shipmentTps;
    }
}
