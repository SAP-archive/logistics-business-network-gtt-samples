package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;

import java.math.BigDecimal;
import java.util.Objects;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.INBOUND_DELIVERY_ITEM_CREATED)
public class PopulateInboundDeliveryItemCreatedCommand implements MilestonePopulateCommand {
    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        BigDecimal quantity = item.getInboundDeliveryItems().stream()
                .filter(Objects::nonNull)
                .map(InboundDeliveryItem::getOrderQuantity)
                .filter(Objects::nonNull)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(quantity);
    }
}
