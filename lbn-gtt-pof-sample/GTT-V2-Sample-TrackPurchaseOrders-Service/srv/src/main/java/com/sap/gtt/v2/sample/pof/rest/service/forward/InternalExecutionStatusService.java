package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@Service
public class InternalExecutionStatusService extends AbstractEventService {

    private static final String EXECUTION_STATUS_COMPLETED = "COMPLETED";
    private static final String EXECUTION_STATUS_IN_TRANSIT = "IN_TRANSIT";
    private static final String POD_EVENT_TYPE = Constants.GTT_MODEL_NAMESPACE + ".Shipment.POD";

    public InternalExecutionStatusService(GTTCoreServiceClient gttCoreServiceClient) {
        super(gttCoreServiceClient);
    }

    public void updateForNotPODInboundDelivery(InboundDeliveryItem inboundDeliveryItem) {
        if (isPodRelevant(inboundDeliveryItem.getId())) {
            return;
        }
        BigDecimal goodsReceiptQuantity = fetchGoodsReceiptQuantity(inboundDeliveryItem.getId());
        if (goodsReceiptQuantity.compareTo(inboundDeliveryItem.getOrderQuantity()) >= 0) {
            inboundDeliveryItem.setExecutionStatusCode(EXECUTION_STATUS_COMPLETED);
        } else {
            inboundDeliveryItem.setExecutionStatusCode(EXECUTION_STATUS_IN_TRANSIT);
        }
    }

    private boolean isPodRelevant(UUID id) {
        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENTS_URL_TEMPLATE, id, PlannedEvent.class);
        return plannedEvents.stream().map(PlannedEvent::getEventType).anyMatch(POD_EVENT_TYPE::equals);
    }

    private BigDecimal fetchGoodsReceiptQuantity(UUID id) {
        List<GoodsReceipt> goodsReceiptList = queryAllEvents(GOODS_RECEIPT_URL_TEMPLATE, id, GoodsReceipt.class);
        return goodsReceiptList.stream().map(GoodsReceipt::getQuantity).reduce(BigDecimal.ZERO, BigDecimal::add);
    }
}
