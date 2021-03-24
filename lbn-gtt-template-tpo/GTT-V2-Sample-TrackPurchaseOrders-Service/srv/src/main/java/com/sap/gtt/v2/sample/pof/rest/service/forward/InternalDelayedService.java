package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import org.springframework.stereotype.Service;

import static com.sap.gtt.v2.sample.pof.constant.Constants.BOOL_MARK;
import static com.sap.gtt.v2.sample.pof.constant.Constants.PROCESS_STATUS_DELAYED;
import static org.apache.commons.lang3.StringUtils.EMPTY;

@Service
public class InternalDelayedService {

    public void processIsDelayed(PurchaseOrder purchaseOrder) {
        boolean orderDelayed = false;
        for (PurchaseOrderItem purchaseOrderItem : purchaseOrder.getPurchaseOrderItemTPs()) {
            boolean isAnyDelayed = purchaseOrderItem.getInboundDeliveryItems().stream()
                    .map(InboundDeliveryItem::getProcessStatusCode)
                    .anyMatch(PROCESS_STATUS_DELAYED::equals);

            purchaseOrderItem.setIsDelayed(isAnyDelayed ? BOOL_MARK : EMPTY);

            orderDelayed = orderDelayed || isAnyDelayed;
        }

        purchaseOrder.setIsDelayed(orderDelayed ? BOOL_MARK : EMPTY);
    }
}
