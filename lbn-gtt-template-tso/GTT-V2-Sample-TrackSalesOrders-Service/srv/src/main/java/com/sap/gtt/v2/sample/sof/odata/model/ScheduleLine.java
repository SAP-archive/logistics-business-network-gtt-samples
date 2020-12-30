package com.sap.gtt.v2.sample.sof.odata.model;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE, name = ScheduleLine.ENTITY_TYPE_NAME)
@EdmEntitySet(name = ScheduleLine.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ScheduleLine {
    public static final String ENTITY_TYPE_NAME = "SalesOrderItemScheduleLines";
    public static final String ENTITY_SET_NAME = "SalesOrderItemScheduleLines";

    @EdmProperty(name = "lineNo")
    private Integer lineNo;

    @EdmProperty(name = "plannedDeliveryDate", type = EdmType.DATE_TIME)
    private Long plannedDeliveryDate;

    @EdmProperty(name = "confirmedQuantity", facets = @EdmFacets(precision = 13, scale = 3))
    private BigDecimal confirmedQuantity;

    @EdmProperty(name = "confirmedQuantityUoM", facets = @EdmFacets(maxLength = 3))
    private String confirmedQuantityUoM;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public Long getPlannedDeliveryDate() {
        return plannedDeliveryDate;
    }

    public void setPlannedDeliveryDate(Long plannedDeliveryDate) {
        this.plannedDeliveryDate = plannedDeliveryDate;
    }

    public BigDecimal getConfirmedQuantity() {
        return confirmedQuantity;
    }

    public void setConfirmedQuantity(BigDecimal confirmedQuantity) {
        this.confirmedQuantity = confirmedQuantity;
    }

    public String getConfirmedQuantityUoM() {
        return confirmedQuantityUoM;
    }

    public void setConfirmedQuantityUoM(String confirmedQuantityUoM) {
        this.confirmedQuantityUoM = confirmedQuantityUoM;
    }
}
