package com.sap.gtt.v2.sample.pof.odata.model;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.math.BigDecimal;

@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
@EdmEntitySet(name = ScheduleLine.ENTITY_SET_NAME, container = Constants.ENTITY_CONTAINER_NAME)
public class ScheduleLine {
    public static final String ENTITY_SET_NAME = "PurchaseOrderItemScheduleLines";

    @EdmKey
    @EdmProperty(name = "lineNo", type = EdmType.INT32)
    private Integer lineNo;

    @EdmProperty(name = "deliveryDate", type = EdmType.DATE_TIME)
    private Long deliveryDate;

    @EdmProperty(name = "scheduledQuantityUoM", facets = @EdmFacets(maxLength = 3))
    private String scheduledQuantityUoM;

    @EdmProperty(name = "scheduledQuantity", facets = @EdmFacets(precision = 13, scale = 2))
    private BigDecimal scheduledQuantity;

    public Integer getLineNo() {
        return lineNo;
    }

    public void setLineNo(Integer lineNo) {
        this.lineNo = lineNo;
    }

    public Long getDeliveryDate() {
        return deliveryDate;
    }

    public void setDeliveryDate(Long deliveryDate) {
        this.deliveryDate = deliveryDate;
    }

    public String getScheduledQuantityUoM() {
        return scheduledQuantityUoM;
    }

    public void setScheduledQuantityUoM(String scheduledQuantityUoM) {
        this.scheduledQuantityUoM = scheduledQuantityUoM;
    }

    public BigDecimal getScheduledQuantity() {
        return scheduledQuantity;
    }

    public void setScheduledQuantity(BigDecimal scheduledQuantity) {
        this.scheduledQuantity = scheduledQuantity;
    }

    @Override
    public String toString() {
        return "ScheduleLine{" +
                "lineNo=" + lineNo +
                ", deliveryDate=" + deliveryDate +
                ", scheduledQuantityUoM='" + scheduledQuantityUoM + '\'' +
                ", scheduledQuantity=" + scheduledQuantity +
                '}';
    }
}
