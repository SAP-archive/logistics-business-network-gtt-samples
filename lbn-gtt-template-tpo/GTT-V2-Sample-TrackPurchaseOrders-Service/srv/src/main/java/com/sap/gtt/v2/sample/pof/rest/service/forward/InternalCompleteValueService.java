package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemTP;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.isBlank;

@Service
public class InternalCompleteValueService extends AbstractEventService {

    private static final String POD_EVENT_TYPE = "Shipment.POD";
    private static final String PART_OF_REPORTED_STATUS = "REPORTED";

    public InternalCompleteValueService(GTTCoreServiceClient gttCoreServiceClient) {
        super(gttCoreServiceClient);
    }

    public void recalculateCompletionValue(PurchaseOrder purchaseOrder) {
        BigDecimal totalCompletionValue = BigDecimal.ZERO;

        for (PurchaseOrderItemTP itemTP : purchaseOrder.getPurchaseOrderItemTPs()) {
            PurchaseOrderItem item = itemTP.getPurchaseOrderItem();
            recalculateCompletionValueForItem(item);

            totalCompletionValue = totalCompletionValue.add(item.getCompletionValue());
        }

        totalCompletionValue = totalCompletionValue.setScale(2, RoundingMode.HALF_UP);
        purchaseOrder.setCompletionValue(totalCompletionValue);
    }

    private void recalculateCompletionValueForItem(PurchaseOrderItem purchaseOrderItem) {
        if (isDeletionLatestEvent(purchaseOrderItem.getId())) {
            purchaseOrderItem.setCompletedQuantity(BigDecimal.ZERO);
            purchaseOrderItem.setCompletionValue(BigDecimal.ZERO);
            return;
        }

        BigDecimal unitPrice = BigDecimal.ZERO.equals(purchaseOrderItem.getNetValue())
                ? BigDecimal.ZERO : purchaseOrderItem.getNetValue().divide(purchaseOrderItem.getOrderQuantity(), MathContext.DECIMAL64);

        BigDecimal itemCompletedQuantity;
        if (purchaseOrderItem.getInboundDeliveryItems().isEmpty()) {
            List<GoodsReceipt> goodsReceipts = queryAllEvents(GOODS_RECEIPT_URL_TEMPLATE, purchaseOrderItem.getId(), GoodsReceipt.class);
            itemCompletedQuantity = goodsReceipts.stream()
                    .map(GoodsReceipt::getQuantity)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        } else {
            itemCompletedQuantity = purchaseOrderItem.getInboundDeliveryItems().stream()
                    .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                    .filter(Objects::nonNull)
                    .map(this::calculateCompletionQuantityByInboundDeliveryItem)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }

        itemCompletedQuantity = itemCompletedQuantity.setScale(2, RoundingMode.HALF_UP);
        BigDecimal itemCompletedValue = itemCompletedQuantity.multiply(unitPrice).setScale(2, RoundingMode.HALF_UP);

        purchaseOrderItem.setCompletedQuantity(itemCompletedQuantity);
        purchaseOrderItem.setCompletionValue(itemCompletedValue);
    }

    private BigDecimal calculateCompletionQuantityByInboundDeliveryItem(InboundDeliveryItem deliveryItem) {
        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENTS_URL_TEMPLATE, deliveryItem.getId(), PlannedEvent.class);
        List<PlannedEvent> podPlannedEvents = plannedEvents.stream()
                .filter(event -> event.getEventType().endsWith(POD_EVENT_TYPE))
                .collect(Collectors.toList());
        if (podPlannedEvents.isEmpty()) {
            return getQuantityByGoodsReceipt(deliveryItem.getId());
        } else if (isAllReported(podPlannedEvents)) {
            return deliveryItem.getOrderQuantity();
        }
        return BigDecimal.ZERO;
    }

    private boolean isAllReported(List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .map(PlannedEvent::getEventStatusCode)
                .allMatch(code -> !isBlank(code) && code.contains(PART_OF_REPORTED_STATUS));
    }

    private BigDecimal getQuantityByGoodsReceipt(UUID id) {
        List<GoodsReceipt> goodsReceipts = queryAllEvents(GOODS_RECEIPT_URL_TEMPLATE, id, GoodsReceipt.class);
        return goodsReceipts.stream()
                .map(GoodsReceipt::getQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

}
