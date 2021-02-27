package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.ConfirmationEvent;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_CONFIRMED)
public class PopulateOrderItemConfirmedCommand extends AbstractEventService implements MilestonePopulateCommand {

    private static final String CONFIRMATION_TYPE = "AB";

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        if (item.getInboundDeliveryItems().isEmpty()) {
            populateBasedOnConfirmationEvents(lane, item);
        } else {
            populateBasedOnDeliveryItems(lane, item);
        }
    }

    private void populateBasedOnDeliveryItems(Lane lane, PurchaseOrderItem item) {
        BigDecimal quantity = item.getInboundDeliveryItems().stream()
                .filter(Objects::nonNull)
                .map(InboundDeliveryItem::getOrderQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(quantity);
    }

    private void populateBasedOnConfirmationEvents(Lane lane, PurchaseOrderItem item) {
        List<ConfirmationEvent> events = queryAllEvents(CONFIRMATION_EVENT_URL_TEMPLATE, item.getId(), ConfirmationEvent.class);
        BigDecimal count = events.stream()
                .filter(it -> CONFIRMATION_TYPE.equals(it.getConfirmType()))
                .map(ConfirmationEvent::getQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(count);
    }
}
