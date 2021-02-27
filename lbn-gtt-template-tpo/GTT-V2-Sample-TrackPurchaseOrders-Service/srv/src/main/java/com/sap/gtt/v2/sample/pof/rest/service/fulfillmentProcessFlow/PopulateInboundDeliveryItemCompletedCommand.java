package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static com.sap.gtt.v2.sample.pof.constant.Constants.POD_EVENT;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.groupGoodsReceiptByProcessId;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.isLastReversal;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static org.apache.commons.lang3.StringUtils.isBlank;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.INBOUND_DELIVERY_ITEM_COMPLETED)
public class PopulateInboundDeliveryItemCompletedCommand extends AbstractEventService implements MilestonePopulateCommand {

    private static final String PART_OF_REPORTED_STATUS = "REPORTED";

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        List<Object> ids = item.getInboundDeliveryItems().stream()
                .map(InboundDeliveryItem::getId)
                .collect(toList());

        List<GoodsReceipt> goodsReceipts = queryAllEvents(GOODS_RECEIPT_URI, EVENT_PROCESS_ID_FILTER_PART, OR_EXPR, ids, GoodsReceipt.class);
        Map<UUID, Set<GoodsReceipt>> geedReceiptsByProcessId = groupGoodsReceiptByProcessId(goodsReceipts);

        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENT_URI, PROCESS_ID_FILTER_PART, OR_EXPR, ids, PlannedEvent.class);
        Map<UUID, List<PlannedEvent>> podPlannedEventsByProcessId = plannedEvents.stream()
                .filter(event -> event.getEventType().endsWith(POD_EVENT))
                .collect(groupingBy(PlannedEvent::getProcessId));

        BigDecimal quantity = item.getInboundDeliveryItems().stream()
                .filter(Objects::nonNull)
                .map(it -> calculateQuantityByDeliveryItem(it, geedReceiptsByProcessId, podPlannedEventsByProcessId))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(quantity);
    }

    private BigDecimal calculateQuantityByDeliveryItem(InboundDeliveryItem deliveryItem, Map<UUID, Set<GoodsReceipt>> goodsReceiptsByProcessId,
                                                       Map<UUID, List<PlannedEvent>> podPlannedEventsByProcessId) {
        List<PlannedEvent> podPlannedEvents = podPlannedEventsByProcessId.getOrDefault(deliveryItem.getId(), Collections.emptyList());
        if (podPlannedEvents.isEmpty()) {
            Set<GoodsReceipt> goodsReceipts = goodsReceiptsByProcessId.getOrDefault(deliveryItem.getId(), Collections.emptySet());
            boolean isReversal = isLastReversal(goodsReceipts);
            return isReversal ? BigDecimal.ZERO : deliveryItem.getOrderQuantity();
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
