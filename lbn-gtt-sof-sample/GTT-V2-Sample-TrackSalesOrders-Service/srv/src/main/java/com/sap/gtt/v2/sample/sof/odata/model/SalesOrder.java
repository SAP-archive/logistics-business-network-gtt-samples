package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = SalesOrder.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class SalesOrder {
    public static final String ENTITY_SET_NAME = "SalesOrder";

    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "salesOrderNo", facets = @EdmFacets(maxLength = 10))
    private String salesOrderNo;

    @EdmProperty(name = "shipToPartyId", facets = @EdmFacets(maxLength = 10))
    private String shipToPartyId;

    @EdmProperty(name = "netValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal netValue;

    @EdmProperty(name = "currency", facets = @EdmFacets(maxLength = 5))
    private String currency;

    @EdmProperty(name = "logicalSystem", facets = @EdmFacets(maxLength = 10))
    private String logicalSystem;

    @EdmProperty(name = "customerReference", facets = @EdmFacets(maxLength = 35))
    private String customerReference;

    @EdmProperty(name = "documentDate", type = EdmType.DATE_TIME)
    private Long documentDate;

    @EdmProperty(name = "incoterms_code")
    @SerializedName("incoterms_code")
    private String incotermsCode;

    @EdmProperty(name = "incotermsLocation", facets = @EdmFacets(maxLength = 70))
    private String incotermsLocation;

    @EdmProperty(name = "incotermsVersion", facets = @EdmFacets(maxLength = 4))
    private String incotermsVersion;

    @EdmProperty(name = "lastChangeDateTime", type = EdmType.DATE_TIME_OFFSET)
    private Long lastChangeDateTime;

    // calculated value
    @EdmProperty(name = "completionValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal completionValue;

    // calculated value
    @EdmProperty(name = "delayedValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal delayedValue;

    @EdmProperty(name = "processStatus_code")
    @SerializedName("processStatus_code")
    private String processStatusCode;

    // virtual attribute for item level filter
    @EdmProperty(name = "vMaterialNo", facets = @EdmFacets(maxLength = 40))
    private String vMaterialNo;

    // virtual attribute for item level filter
    @EdmProperty(name = "vMaterialDescription", facets = @EdmFacets(maxLength = 40))
    private String vMaterialDescription;

    // virtual attribute for item level filter
    @EdmProperty(name = "vShipmentNo", facets = @EdmFacets(maxLength = 10))
    private String vShipmentNo;

    @EdmNavigationProperty(name = "salesOrderItemTPs", toType = SalesOrderItemTP.class, toMultiplicity = EdmNavigationProperty.Multiplicity.MANY)
    private List<SalesOrderItemTP> salesOrderItemTPs;

    @EdmNavigationProperty(name = "incoterms", toType = Incoterms.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private Incoterms incoterms;

    @EdmNavigationProperty(name = "processStatus", toType = ProcessStatus.class, toMultiplicity =
            EdmNavigationProperty.Multiplicity.ONE)
    private ProcessStatus processStatus;
    @EdmProperty(name = "altKey")
    private String altKey;

    @EdmProperty(name = "completedAndLateValue", facets = @EdmFacets(precision = 15, scale = 2))
    private BigDecimal completedAndLateValue;

    @EdmProperty(name = "isCompleted")
    private Boolean isCompleted;

    @EdmProperty(name = "isDelayed")
    private Boolean isDelayed;

    @EdmProperty(name = "trackingIdType")
    private String trackingIdType;

    public String getTrackingIdType() {
        return trackingIdType;
    }

    public void setTrackingIdType(String trackingIdType) {
        this.trackingIdType = trackingIdType;
    }

    public Boolean getDelayed() {
        return isDelayed;
    }

    public void setDelayed(Boolean delayed) {
        isDelayed = delayed;
    }

    public BigDecimal getCompletedAndLateValue() {
        return completedAndLateValue;
    }

    public void setCompletedAndLateValue(BigDecimal completedAndLateValue) {
        this.completedAndLateValue = completedAndLateValue;
    }

    public Boolean getCompleted() {
        return isCompleted;
    }

    public void setCompleted(Boolean completed) {
        isCompleted = completed;
    }

    public String getAltKey() {
        return altKey;
    }

    public void setAltKey(String altKey) {
        this.altKey = altKey;
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public String getSalesOrderNo() {
        return salesOrderNo;
    }

    public void setSalesOrderNo(String salesOrderNo) {
        this.salesOrderNo = salesOrderNo;
    }

    public String getShipToPartyId() {
        return shipToPartyId;
    }

    public void setShipToPartyId(String shipToPartyId) {
        this.shipToPartyId = shipToPartyId;
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

    public String getLogicalSystem() {
        return logicalSystem;
    }

    public void setLogicalSystem(String logicalSystem) {
        this.logicalSystem = logicalSystem;
    }

    public String getCustomerReference() {
        return customerReference;
    }

    public void setCustomerReference(String customerReference) {
        this.customerReference = customerReference;
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

    public String getIncotermsLocation() {
        return incotermsLocation;
    }

    public void setIncotermsLocation(String incotermsLocation) {
        this.incotermsLocation = incotermsLocation;
    }

    public String getIncotermsVersion() {
        return incotermsVersion;
    }

    public void setIncotermsVersion(String incotermsVersion) {
        this.incotermsVersion = incotermsVersion;
    }

    public Long getLastChangeDateTime() {
        return lastChangeDateTime;
    }

    public void setLastChangeDateTime(Long lastChangeDateTime) {
        this.lastChangeDateTime = lastChangeDateTime;
    }

    public BigDecimal getCompletionValue() {
        return completionValue;
    }

    public void setCompletionValue(BigDecimal completionValue) {
        this.completionValue = completionValue;
    }

    public BigDecimal getDelayedValue() {
        return delayedValue;
    }

    public void setDelayedValue(BigDecimal delayedValue) {
        this.delayedValue = delayedValue;
    }

    public String getvMaterialNo() {
        return vMaterialNo;
    }

    public void setvMaterialNo(String vMaterialNo) {
        this.vMaterialNo = vMaterialNo;
    }

    public String getvMaterialDescription() {
        return vMaterialDescription;
    }

    public void setvMaterialDescription(String vMaterialDescription) {
        this.vMaterialDescription = vMaterialDescription;
    }

    public String getvShipmentNo() {
        return vShipmentNo;
    }

    public void setvShipmentNo(String vShipmentNo) {
        this.vShipmentNo = vShipmentNo;
    }

    public List<SalesOrderItemTP> getSalesOrderItemTPs() {
        return salesOrderItemTPs;
    }

    public void setSalesOrderItemTPs(List<SalesOrderItemTP> salesOrderItemTPs) {
        this.salesOrderItemTPs = salesOrderItemTPs;
    }

    public Incoterms getIncoterms() {
        return incoterms;
    }

    public void setIncoterms(Incoterms incoterms) {
        this.incoterms = incoterms;
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
}
