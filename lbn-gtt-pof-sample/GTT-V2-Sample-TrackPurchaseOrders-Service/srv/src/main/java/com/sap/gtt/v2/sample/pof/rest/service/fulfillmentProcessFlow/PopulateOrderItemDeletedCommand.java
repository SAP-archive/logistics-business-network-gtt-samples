package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_DELETED)
public class PopulateOrderItemDeletedCommand implements MilestonePopulateCommand {
    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        lane.setCount(lane.getTotal());
    }
}
