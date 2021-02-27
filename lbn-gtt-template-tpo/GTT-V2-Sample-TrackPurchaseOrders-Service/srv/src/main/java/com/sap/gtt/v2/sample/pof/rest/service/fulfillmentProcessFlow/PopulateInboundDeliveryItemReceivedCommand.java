package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.groupGoodsReceiptByProcessId;
import static com.sap.gtt.v2.sample.pof.utils.FulfillmentProcessUtils.isLastReversal;

@MilestoneCommand(FulfillmentProcessMilestoneEnum.INBOUND_DELIVERY_ITEM_RECEIPT)
public class PopulateInboundDeliveryItemReceivedCommand extends AbstractEventService implements MilestonePopulateCommand {

    @Override
    public void populateMilestone(Lane lane, PurchaseOrderItem item) {
        BigDecimal quantity = fetchReceivedQuantity(item.getInboundDeliveryItems());
        lane.setCount(quantity);
    }

    private BigDecimal fetchReceivedQuantity(List<InboundDeliveryItem> itemTPs) {
        Map<Object, InboundDeliveryItem> map = itemTPs.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.toMap(InboundDeliveryItem::getId, Function.identity()));

        return map.isEmpty() ? BigDecimal.ZERO
                : fetchGoodsReceiptQuantity(map);
    }

    private BigDecimal fetchGoodsReceiptQuantity(Map<Object, InboundDeliveryItem> deliveryItemById) {
        List<GoodsReceipt> goodsReceipts =
                queryAllEvents(GOODS_RECEIPT_URI, EVENT_PROCESS_ID_FILTER_PART, OR_EXPR, new ArrayList<>(deliveryItemById.keySet()), GoodsReceipt.class);
        Map<UUID, Set<GoodsReceipt>> goodsReceiptByProcessId = groupGoodsReceiptByProcessId(goodsReceipts);
        return deliveryItemById.values().stream()
                .map(inboundDeliveryItem -> getQuantityByGoodsReceipt(inboundDeliveryItem, goodsReceiptByProcessId))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private BigDecimal getQuantityByGoodsReceipt(InboundDeliveryItem item, Map<UUID, Set<GoodsReceipt>> goodsReceiptByProcessId) {
        Set<GoodsReceipt> goodsReceiptSet = goodsReceiptByProcessId.getOrDefault(item.getId(), Collections.emptySet());
        boolean isReversal = isLastReversal(goodsReceiptSet);
        return isReversal ? BigDecimal.ZERO : item.getOrderQuantity();
    }
}