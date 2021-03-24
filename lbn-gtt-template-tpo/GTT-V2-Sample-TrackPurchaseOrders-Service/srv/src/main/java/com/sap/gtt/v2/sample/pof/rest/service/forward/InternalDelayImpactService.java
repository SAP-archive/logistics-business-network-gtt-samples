package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

@Service
public class InternalDelayImpactService {

    public void updateDelayImpact(PurchaseOrder purchaseOrder) {
        BigDecimal totalDelayValue = BigDecimal.ZERO;

        for (PurchaseOrderItem item : purchaseOrder.getPurchaseOrderItemTPs()) {
            totalDelayValue = totalDelayValue.add(calcDelayValueOfPurchaseOrderItem(item));
        }

        totalDelayValue = totalDelayValue.setScale(2, RoundingMode.HALF_UP);
        purchaseOrder.setDelayedValue(totalDelayValue);
    }

    private BigDecimal calcDelayValueOfPurchaseOrderItem(PurchaseOrderItem purchaseOrderItem) {
        BigDecimal unitPrice = BigDecimal.ZERO.equals(purchaseOrderItem.getNetValue())
                ? BigDecimal.ZERO : purchaseOrderItem.getNetValue().divide(purchaseOrderItem.getOrderQuantity(), MathContext.DECIMAL64);

        BigDecimal delayQuantity = purchaseOrderItem.getInboundDeliveryItems().stream()
                .filter(it -> Constants.PROCESS_STATUS_DELAYED.equals(it.getProcessStatusCode()))
                .map(InboundDeliveryItem::getOrderQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        delayQuantity = delayQuantity.setScale(2, RoundingMode.HALF_UP);
        BigDecimal delayValue = delayQuantity.multiply(unitPrice).setScale(2, RoundingMode.HALF_UP);

        purchaseOrderItem.setDelayedValue(delayValue);
        purchaseOrderItem.setDelayedQuantity(delayQuantity);
        return delayValue;
    }
}
