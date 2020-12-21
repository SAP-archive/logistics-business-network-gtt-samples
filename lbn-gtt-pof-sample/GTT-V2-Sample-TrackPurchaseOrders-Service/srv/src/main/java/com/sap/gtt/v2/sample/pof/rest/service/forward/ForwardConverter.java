package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.domain.InboundDeliveryItemEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderEvent;
import com.sap.gtt.v2.sample.pof.domain.PurchaseOrderItemEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.stereotype.Service;

@Service
public class ForwardConverter {

    public PurchaseOrderEvent convertPurchaseOrder(PurchaseOrder purchaseOrder) {
        PurchaseOrderEvent event = new PurchaseOrderEvent();
        event.setAltKey(purchaseOrder.getAltKey());
        event.setCompletionValue(purchaseOrder.getCompletionValue());
        event.setActualBusinessTimestamp(POFUtils.getTimeString());
        event.setActualBusinessTimeZone("UTC");
        event.setPurchaseOrderNo(purchaseOrder.getPurchaseOrderNo());
        return event;
    }

    public PurchaseOrderItemEvent convertPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem) {
        PurchaseOrderItemEvent event = new PurchaseOrderItemEvent();
        event.setAltKey(purchaseOrderItem.getAltKey());
        event.setCompletionValue(purchaseOrderItem.getCompletionValue());
        event.setCompletedQuantity(purchaseOrderItem.getCompletedQuantity());
        event.setActualBusinessTimestamp(POFUtils.getTimeString());
        event.setActualBusinessTimeZone("UTC");
        event.setPurchaseOrderNo(purchaseOrderItem.getPurchaseOrderNo());
        event.setItemNo(purchaseOrderItem.getItemNo());
        return event;
    }

    public InboundDeliveryItemEvent convertInboundDeliveryItem(InboundDeliveryItem inboundDeliveryItem) {
        InboundDeliveryItemEvent event = new InboundDeliveryItemEvent();
        event.setAltKey(inboundDeliveryItem.getAltKey());
        event.setExecutionStatusCode(inboundDeliveryItem.getExecutionStatusCode());
        event.setActualBusinessTimestamp(POFUtils.getTimeString());
        event.setActualBusinessTimeZone("UTC");
        event.setInboundDeliveryNo(inboundDeliveryItem.getInboundDeliveryNo());
        event.setItemNo(inboundDeliveryItem.getItemNo());
        return event;
    }
}
