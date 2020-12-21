package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static org.apache.commons.lang3.StringUtils.isBlank;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.INBOUND_DELIVERY_ITEM_COMPLETED)
public class PopulateInboundDeliveryItemCompletedCommand extends AbstractEventService implements MilestonePopulateCommand {

    private static final String POD_EVENT_TYPE = "Shipment.POD";
    private static final String PART_OF_REPORTED_STATUS = "REPORTED";

    public PopulateInboundDeliveryItemCompletedCommand(GTTCoreServiceClient gttCoreServiceClient) {
        super(gttCoreServiceClient);
    }

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        BigDecimal quantity = item.getInboundDeliveryItems().stream()
                .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                .filter(Objects::nonNull)
                .map(this::calculateQuantityByDeliveryItem)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(quantity);
    }

    private BigDecimal calculateQuantityByDeliveryItem(InboundDeliveryItem deliveryItem) {
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
