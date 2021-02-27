package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.Constants.POD_EVENT;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.groupGoodsReceiptByProcessId;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.isLastReversal;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.StringUtils.isBlank;

@Service
public class InternalCompleteValueService extends AbstractEventService {

    private static final String PART_OF_REPORTED_STATUS = "REPORTED";

    public void recalculateCompletionValue(PurchaseOrder purchaseOrder) {
        BigDecimal totalCompletionValue = BigDecimal.ZERO;

        for (PurchaseOrderItem item : purchaseOrder.getPurchaseOrderItemTPs()) {
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
            List<InboundDeliveryItem> inboundDeliveryItems = purchaseOrderItem.getInboundDeliveryItems().stream()
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            itemCompletedQuantity = calculateCompletionQuantityByDeliveryItems(inboundDeliveryItems);
        }

        itemCompletedQuantity = itemCompletedQuantity.setScale(2, RoundingMode.HALF_UP);
        BigDecimal itemCompletedValue = itemCompletedQuantity.multiply(unitPrice).setScale(2, RoundingMode.HALF_UP);

        purchaseOrderItem.setCompletedQuantity(itemCompletedQuantity);
        purchaseOrderItem.setCompletionValue(itemCompletedValue);
    }

    private BigDecimal calculateCompletionQuantityByDeliveryItems(List<InboundDeliveryItem> inboundDeliveryItems) {
        List<Object> ids = inboundDeliveryItems.stream().map(InboundDeliveryItem::getId).collect(toList());

        List<GoodsReceipt> goodsReceipts = queryAllEvents(GOODS_RECEIPT_URI, EVENT_PROCESS_ID_FILTER_PART, OR_EXPR, ids, GoodsReceipt.class);
        Map<UUID, Set<GoodsReceipt>> goodReceiptsByProcessId = groupGoodsReceiptByProcessId(goodsReceipts);

        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENT_URI, PROCESS_ID_FILTER_PART, OR_EXPR, ids, PlannedEvent.class);
        Map<UUID, List<PlannedEvent>> podPlannedEventsByProcessId = plannedEvents.stream()
                .filter(event -> event.getEventType().endsWith(POD_EVENT))
                .collect(groupingBy(PlannedEvent::getProcessId));

        return inboundDeliveryItems.stream()
                .map(it -> calculateQuantityByDeliveryItem(it, goodReceiptsByProcessId, podPlannedEventsByProcessId))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private BigDecimal calculateQuantityByDeliveryItem(InboundDeliveryItem deliveryItem, Map<UUID, Set<GoodsReceipt>> goodsReceiptsByProcessId,
                                                       Map<UUID, List<PlannedEvent>> podPlannedEventsByProcessId) {
        List<PlannedEvent> podPlannedEvents = podPlannedEventsByProcessId.getOrDefault(deliveryItem.getId(), Collections.emptyList());
        if (podPlannedEvents.isEmpty()) {
            Set<GoodsReceipt> goodsReceipts = goodsReceiptsByProcessId.getOrDefault(deliveryItem.getId(), Collections.emptySet());
            return isLastReversal(goodsReceipts) ? BigDecimal.ZERO : deliveryItem.getOrderQuantity();
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
}
