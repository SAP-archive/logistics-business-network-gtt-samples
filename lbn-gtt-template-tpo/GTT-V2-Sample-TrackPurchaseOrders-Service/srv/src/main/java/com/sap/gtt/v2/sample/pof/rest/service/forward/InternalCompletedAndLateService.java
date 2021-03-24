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
public class InternalCompletedAndLateService {

    public void updateCompletedAndLate(PurchaseOrder purchaseOrder) {
        BigDecimal totalCompletedAndLate = BigDecimal.ZERO;

        for (PurchaseOrderItem item : purchaseOrder.getPurchaseOrderItemTPs()) {
            totalCompletedAndLate = totalCompletedAndLate.add(calculateCompletedAndLateOfPOItem(item));
        }

        totalCompletedAndLate = totalCompletedAndLate.setScale(2, RoundingMode.HALF_UP);
        purchaseOrder.setCompletedAndLateValue(totalCompletedAndLate);
    }

    private BigDecimal calculateCompletedAndLateOfPOItem(PurchaseOrderItem purchaseOrderItem) {
        BigDecimal unitPrice = BigDecimal.ZERO.equals(purchaseOrderItem.getNetValue())
                ? BigDecimal.ZERO : purchaseOrderItem.getNetValue().divide(purchaseOrderItem.getOrderQuantity(), MathContext.DECIMAL64);

        BigDecimal completedAndLateQuantity = purchaseOrderItem.getInboundDeliveryItems().stream()
                .filter(it -> Constants.EXECUTION_STATUS_COMPLETED.equals(it.getExecutionStatusCode()))
                .filter(it -> isLateOrDelayed(it.getProcessStatusCode()))
                .map(InboundDeliveryItem::getOrderQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        completedAndLateQuantity = completedAndLateQuantity.setScale(2, RoundingMode.HALF_UP);
        BigDecimal completedAndLateValue = completedAndLateQuantity.multiply(unitPrice).setScale(2, RoundingMode.HALF_UP);

        purchaseOrderItem.setCompletedAndLateValue(completedAndLateValue);
        purchaseOrderItem.setCompletedAndLateQuantity(completedAndLateQuantity);
        return completedAndLateValue;
    }

    private boolean isLateOrDelayed(String status) {
        return Constants.PROCESS_STATUS_DELAYED.equals(status) || Constants.PROCESS_STATUS_LATE.equals(status);
    }
}
