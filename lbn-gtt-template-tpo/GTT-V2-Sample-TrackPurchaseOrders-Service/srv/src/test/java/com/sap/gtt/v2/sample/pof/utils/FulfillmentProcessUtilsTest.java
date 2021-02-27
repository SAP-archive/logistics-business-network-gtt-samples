package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.domain.GoodsReceipt;
import org.assertj.core.api.Assertions;
import org.junit.Test;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.Objects.nonNull;

public class FulfillmentProcessUtilsTest {

    @Test
    public void groupGoodsReceiptByProcessId() {
        String stringFromResource = POFUtils.getStringFromResource("odata/fulfillment_process_utils-goods_receipts.json");
        List<GoodsReceipt> entities = ODataUtils.readEntitySet(stringFromResource, GoodsReceipt.class).getResults();

        Map<UUID, Set<GoodsReceipt>> uuidSetMap = FulfillmentProcessUtils.groupGoodsReceiptByProcessId(entities);

        List<GoodsReceipt> actual = uuidSetMap.values().stream().flatMap(Collection::stream).distinct().collect(Collectors.toList());
        List<GoodsReceipt> expected = entities.stream()
                .filter(it -> nonNull(it.getEventProcesses()) && !it.getEventProcesses().isEmpty())
                .collect(Collectors.toList());

        Assertions.assertThat(actual).hasSameSizeAs(expected);
    }
}