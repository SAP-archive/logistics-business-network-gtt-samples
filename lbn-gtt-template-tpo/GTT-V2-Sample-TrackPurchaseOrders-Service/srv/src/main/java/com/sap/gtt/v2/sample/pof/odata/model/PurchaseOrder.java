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
@EdmEntitySet(name = PurchaseOrder.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class PurchaseOrder {

    public static final String ENTITY_SET_NAME = "PurchaseOrder";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "altKey")
    private String altKey;

    @EdmProperty(name = "purchaseOrderNo", facets = @EdmFacets(maxLength = 10))
    private String purchaseOrderNo;

    @EdmProperty(name = "supplierId", facets = @EdmFacets(maxLength = 255))
    private String supplierId;

    @EdmProperty(name = "plannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long plannedDeliveryDate;

    @EdmProperty(name = "netValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal netValue;

    @EdmProperty(name = "currency", facets = @EdmFacets(maxLength = 5))
    private String currency;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 4))
    private String incotermsVersion;

    @EdmProperty(name = "incotermsLocation", facets = @EdmFacets(maxLength = 255))
    private String incotermsLocation;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "totalDeliveredValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal totalDeliveredValue;

    @EdmProperty(name = "totalDelayedValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal totalDelayedValue;

    @EdmProperty(name = "receivingLocationId", facets = @EdmFacets(maxLength = 255))
    private String receivingLocationId;

    @EdmProperty(name = "receivingLocationDescription", facets = @EdmFacets(maxLength = 255))
    private String receivingLocationDescription;

    @EdmNavigationProperty(name = "supplierLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType supplierLocationType;

    // calculated value
    @EdmProperty(name = "completionValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal completionValue;

    @EdmProperty(name = "trackingIdType", facets = @EdmFacets(maxLength = 40))
    private String trackingIdType;

    @EdmProperty(name = "partyId")
    private String partyId;

    @EdmProperty(name = "incoterms_code")
    @SerializedName("incoterms_code")
    private String incotermsCode;

    @EdmProperty(name = "receivingLocationType_code")
    @SerializedName("receivingLocationType_code")
    private String receivingLocationTypeCode;

    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;

    @EdmProperty(name = "supplierDescription", facets = @EdmFacets(maxLength = 255))
    private String supplierDescription;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "receivingLocationType", toType = LocationType.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private LocationType receivingLocationType;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;

    @EdmNavigationProperty(name = "purchaseOrderItemTPs", toType = PurchaseOrderItemTP.class,
            toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<PurchaseOrderItemTP> purchaseOrderItemTPs;

    @EdmNavigationProperty(name = Constants.RECEIVING_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO receivingLocation;

    @EdmNavigationProperty(name = Constants.SUPPLIER_LOCATION, toMultiplicity = EdmNavigationProperty.Multiplicity.ONE, toType = LocationDTO.class)
    private LocationDTO supplierLocation;

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

    public String getPurchaseOrderNo() {
        return purchaseOrderNo;
    }

    public void setPurchaseOrderNo(String purchaseOrderNo) {
        this.purchaseOrderNo = purchaseOrderNo;
    }

    public String getSupplierId() {
        return supplierId;
    }

    public void setSupplierId(String supplierId) {
        this.supplierId = supplierId;
    }

    public Long getPlannedDeliveryDate() {
        return plannedDeliveryDate;
    }

    public void setPlannedDeliveryDate(Long plannedDeliveryDate) {
        this.plannedDeliveryDate = plannedDeliveryDate;
    }

    public BigDecimal getNetValue() {
        return netValue;
    }

    public void setNetValue(BigDecimal netValue) {
        this.netValue = netValue;
    }

    public String getCurrency() {
        return currency;
    }

    public void setCurrency(String currency) {
        this.currency = currency;
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

    public String getLogicalSystem() {
        return logicalSystem;
    }

    public void setLogicalSystem(String logicalSystem) {
        this.logicalSystem = logicalSystem;
    }

    public BigDecimal getTotalDeliveredValue() {
        return totalDeliveredValue;
    }

    public void setTotalDeliveredValue(BigDecimal totalDeliveredValue) {
        this.totalDeliveredValue = totalDeliveredValue;
    }

    public BigDecimal getTotalDelayedValue() {
        return totalDelayedValue;
    }

    public void setTotalDelayedValue(BigDecimal totalDelayedValue) {
        this.totalDelayedValue = totalDelayedValue;
    }

    public String getReceivingLocationId() {
        return receivingLocationId;
    }

    public void setReceivingLocationId(String receivingLocationId) {
        this.receivingLocationId = receivingLocationId;
    }

    public String getReceivingLocationDescription() {
        return receivingLocationDescription;
    }

    public void setReceivingLocationDescription(String receivingLocationDescription) {
        this.receivingLocationDescription = receivingLocationDescription;
    }

    public BigDecimal getCompletionValue() {
        return completionValue;
    }

    public void setCompletionValue(BigDecimal completionValue) {
        this.completionValue = completionValue;
    }

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public String getPartyId() {
        return partyId;
    }

    public void setPartyId(String partyId) {
        this.partyId = partyId;
    }

    public String getIncotermsCode() {
        return incotermsCode;
    }

    public void setIncotermsCode(String incotermsCode) {
        this.incotermsCode = incotermsCode;
    }

    public String getReceivingLocationTypeCode() {
        return receivingLocationTypeCode;
    }

    public void setReceivingLocationTypeCode(String receivingLocationTypeCode) {
        this.receivingLocationTypeCode = receivingLocationTypeCode;
    }

    public String getProcessStatusCode() {
        return processStatusCode;
    }

    public void setProcessStatusCode(String processStatusCode) {
        this.processStatusCode = processStatusCode;
    }

    public String getSupplierDescription() {
        return supplierDescription;
    }

    public void setSupplierDescription(String supplierDescription) {
        this.supplierDescription = supplierDescription;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
    }

    public LocationType getReceivingLocationType() {
        return receivingLocationType;
    }

    public void setReceivingLocationType(LocationType receivingLocationType) {
        this.receivingLocationType = receivingLocationType;
    }

    public LocationType getSupplierLocationType() {
        return supplierLocationType;
    }

    public void setSupplierLocationType(LocationType supplierLocationType) {
        this.supplierLocationType = supplierLocationType;
    }

    public ProcessStatus getProcessStatus() {
        return processStatus;
    }

    public void setProcessStatus(ProcessStatus processStatus) {
        this.processStatus = processStatus;
    }

    public List<PurchaseOrderItemTP> getPurchaseOrderItemTPs() {
        return purchaseOrderItemTPs;
    }

    public void setPurchaseOrderItemTPs(List<PurchaseOrderItemTP> purchaseOrderItemTPs) {
        this.purchaseOrderItemTPs = purchaseOrderItemTPs;
    }

    public LocationDTO getReceivingLocation() {
        return receivingLocation;
    }

    public void setReceivingLocation(LocationDTO receivingLocation) {
        this.receivingLocation = receivingLocation;
    }

    public LocationDTO getSupplierLocation() {
        return supplierLocation;
    }

    public void setSupplierLocation(LocationDTO supplierLocation) {
        this.supplierLocation = supplierLocation;
    }

    @Override
    public String toString() {
        return "PurchaseOrder{" +
                "id=" + id +
                ", altKey='" + altKey + '\'' +
                ", purchaseOrderNo='" + purchaseOrderNo + '\'' +
                ", supplierId='" + supplierId + '\'' +
                ", plannedDeliveryDate=" + plannedDeliveryDate +
                ", netValue=" + netValue +
                ", currency='" + currency + '\'' +
                ", incotermsVersion='" + incotermsVersion + '\'' +
                ", incotermsLocation='" + incotermsLocation + '\'' +
                ", logicalSystem='" + logicalSystem + '\'' +
                ", totalDeliveredValue=" + totalDeliveredValue +
                ", totalDelayedValue=" + totalDelayedValue +
                ", receivingLocationId='" + receivingLocationId + '\'' +
                ", receivingLocationDescription='" + receivingLocationDescription + '\'' +
                ", supplierLocationType=" + supplierLocationType +
                ", completionValue=" + completionValue +
                ", trackingIdType='" + trackingIdType + '\'' +
                ", partyId='" + partyId + '\'' +
                ", incotermsCode='" + incotermsCode + '\'' +
                ", receivingLocationTypeCode='" + receivingLocationTypeCode + '\'' +
                ", processStatusCode='" + processStatusCode + '\'' +
                ", supplierDescription='" + supplierDescription + '\'' +
                ", incoterms=" + incoterms +
                ", receivingLocationType=" + receivingLocationType +
                ", processStatus=" + processStatus +
                ", purchaseOrderItemTPs=" + purchaseOrderItemTPs +
                ", receivingLocation=" + receivingLocation +
                ", supplierLocation=" + supplierLocation +
                '}';
    }
}
