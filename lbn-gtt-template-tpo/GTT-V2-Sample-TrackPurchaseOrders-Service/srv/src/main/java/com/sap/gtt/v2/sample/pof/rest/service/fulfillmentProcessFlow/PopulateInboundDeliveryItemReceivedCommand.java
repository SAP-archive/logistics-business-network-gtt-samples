package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.INBOUND_DELIVERY_ITEM_RECEIPT)
public class PopulateInboundDeliveryItemReceivedCommand extends AbstractEventService implements MilestonePopulateCommand {
    public PopulateInboundDeliveryItemReceivedCommand(GTTCoreServiceClient gttCoreServiceClient) {
        super(gttCoreServiceClient);
    }

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        BigDecimal quantity = fetchReceivedQuantity(item.getInboundDeliveryItems());
        lane.setCount(quantity);
    }

    public BigDecimal fetchReceivedQuantity(List<PurchaseOrderItemInboundDeliveryItemTP> itemTPs) {
        List<Object> ids = itemTPs.stream()
                .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                .filter(Objects::nonNull)
                .map(InboundDeliveryItem::getId)
                .collect(Collectors.toList());

        return ids.isEmpty() ? BigDecimal.ZERO
                : fetchGoodsReceiptQuantity(ids);
    }

    public BigDecimal fetchGoodsReceiptQuantity(List<Object> ids) {
        return queryAllEvents(GOODS_RECEIPT_URI, EVENT_PROCESS_ID_FILTER_PART, OR_EXPR, ids, GoodsReceipt.class)
                .stream()
                .map(GoodsReceipt::getQuantity)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }
}
