package com.sap.gtt.v2.sample.pof.utils;

import java.util.Arrays;
import java.util.List;

public class ForwardUtils {
    private static final List<String> EVENT_TYPE_WHITE_LIST_FOR_COMPLETION = Arrays.asList(
            "PurchaseOrderItem.GoodsReceipt",
            "Shipment.POD"
    );

    private ForwardUtils() {
    }

    public static boolean isActualEventForCompletion(String eventType) {
        String[] parts = eventType.split("\\.");
        String eventName = parts[parts.length - 1];
        return EVENT_TYPE_WHITE_LIST_FOR_COMPLETION.stream().anyMatch(typeName -> typeName.endsWith(eventName));
    }
}
