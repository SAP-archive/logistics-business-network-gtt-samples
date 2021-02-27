package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;

import java.math.BigDecimal;
import java.util.List;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_RECEIPT)
public class PopulateOrderItemReceivedCommand extends AbstractEventService implements MilestonePopulateCommand {

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        List<GoodsReceipt> goodsReceipts = queryAllEvents(GOODS_RECEIPT_URL_TEMPLATE, item.getId(), GoodsReceipt.class);
        BigDecimal count = goodsReceipts.stream().map(GoodsReceipt::getQuantity).reduce(BigDecimal.ZERO, BigDecimal::add);
        lane.setCount(count);
    }
}
