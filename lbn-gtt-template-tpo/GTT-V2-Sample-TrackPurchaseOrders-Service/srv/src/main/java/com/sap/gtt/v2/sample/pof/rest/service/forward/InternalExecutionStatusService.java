package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXECUTION_STATUS_COMPLETED;
import static com.sap.gtt.v2.sample.pof.constant.Constants.EXECUTION_STATUS_IN_TRANSIT;
import static com.sap.gtt.v2.sample.pof.constant.Constants.POD_EVENT;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.isLastReversal;

@Service
public class InternalExecutionStatusService extends AbstractEventService {

    public void updateForNotPODInboundDelivery(InboundDeliveryItem inboundDeliveryItem) {
        if (isPodRelevant(inboundDeliveryItem.getId())) {
            return;
        }
        BigDecimal goodsReceiptQuantity = fetchGoodsReceiptQuantity(inboundDeliveryItem.getId(), inboundDeliveryItem.getOrderQuantity());
        if (goodsReceiptQuantity.compareTo(inboundDeliveryItem.getOrderQuantity()) >= 0) {
            inboundDeliveryItem.setExecutionStatusCode(EXECUTION_STATUS_COMPLETED);
        } else {
            inboundDeliveryItem.setExecutionStatusCode(EXECUTION_STATUS_IN_TRANSIT);
        }
    }

    private boolean isPodRelevant(UUID id) {
        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENTS_URL_TEMPLATE, id, PlannedEvent.class);
        return plannedEvents.stream()
                .map(PlannedEvent::getEventType)
                .anyMatch(eventType -> POFUtils.isEventTypesEqual(POD_EVENT, eventType));
    }

    private BigDecimal fetchGoodsReceiptQuantity(UUID id, BigDecimal orderQuantity) {
        List<GoodsReceipt> goodsReceiptList = queryAllEvents(GOODS_RECEIPT_URL_TEMPLATE, id, GoodsReceipt.class);
        boolean isReversal = isLastReversal(goodsReceiptList);
        return isReversal ? BigDecimal.ZERO : orderQuantity;
    }
}
