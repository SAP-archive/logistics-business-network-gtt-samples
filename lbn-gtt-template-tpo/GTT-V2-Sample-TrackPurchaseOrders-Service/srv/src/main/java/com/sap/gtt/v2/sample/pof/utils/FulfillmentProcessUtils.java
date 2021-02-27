package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static com.sap.gtt.v2.sample.pof.constant.Constants.MARK_REVERSAL;
import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;

public class FulfillmentProcessUtils {

    private FulfillmentProcessUtils() {
    }

    public static Map<UUID, Set<GoodsReceipt>> groupGoodsReceiptByProcessId(List<GoodsReceipt> goodsReceipts) {
        return goodsReceipts.stream().collect(
                HashMap::new,
                (map, goodReceipt) -> goodReceipt.getEventProcesses()
                        .forEach(it -> map.compute(it.getProcessId(),
                                (key, value) -> {
                                    value = isNull(value) ? new HashSet<>() : value;
                                    value.add(goodReceipt);
                                    return value;
                                })
                        ),
                (leftMap, rightMap) -> rightMap.forEach(
                        (processId, goodsReceiptSet) -> leftMap.compute(processId,
                                (key, value) -> {
                                    value = isNull(value) ? new HashSet<>() : value;
                                    value.addAll(goodsReceiptSet);
                                    return value;
                                })
                ));
    }

    public static boolean isLastReversal(Collection<GoodsReceipt> goodsReceipts) {
        String reversal = goodsReceipts.stream()
                .max(Comparator.comparingLong(Event::getActualBusinessTimestamp))
                .map(it -> isNull(it.getReversal()) ? EMPTY : it.getReversal())
                .orElse(MARK_REVERSAL);
        return MARK_REVERSAL.equals(reversal);
    }
}
