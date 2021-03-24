package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static java.util.Objects.isNull;
import static java.util.function.Function.identity;

public class ForwardUtils {
    private static final List<String> EVENT_TYPE_WHITE_LIST_FOR_COMPLETION = Arrays.asList(
            "PurchaseOrderItem.GoodsReceipt",
            "Shipment.POD"
    );

    private ForwardUtils() {
    }

    public static boolean isActualEventForFlow(String eventType) {
        String[] parts = eventType.split("\\.");
        String eventName = parts[parts.length - 1];
        return EVENT_TYPE_WHITE_LIST_FOR_COMPLETION.stream().anyMatch(typeName -> typeName.endsWith(eventName));
    }

    public static PurchaseOrder cloneForUpdatable(PurchaseOrder purchaseOrder) {
        PurchaseOrder result = new PurchaseOrder();
        result.setId(purchaseOrder.getId());
        result.setIsDelayed(purchaseOrder.getIsDelayed());
        result.setCompletionValue(purchaseOrder.getCompletionValue());
        result.setDelayedValue(purchaseOrder.getDelayedValue());
        result.setCompletedAndLateValue(purchaseOrder.getCompletedAndLateValue());
        return result;
    }

    public static Map<UUID, PurchaseOrderItem> cloneOrderItemsAndGroupForUpdatable(List<PurchaseOrderItem> purchaseOrderItems) {
        return purchaseOrderItems.stream()
                .map(it -> {
                    PurchaseOrderItem result = new PurchaseOrderItem();
                    result.setId(it.getId());
                    result.setIsDelayed(it.getIsDelayed());
                    result.setCompletedQuantity(it.getCompletedQuantity());
                    result.setCompletionValue(it.getCompletionValue());
                    result.setDelayedQuantity(it.getDelayedQuantity());
                    result.setDelayedValue(it.getDelayedValue());
                    result.setCompletedAndLateQuantity(it.getCompletedAndLateQuantity());
                    result.setCompletedAndLateValue(it.getCompletedAndLateValue());
                    return result;
                }).collect(Collectors.toMap(PurchaseOrderItem::getId, identity()));
    }

    public static Map<UUID, InboundDeliveryItem> cloneDeliveryItemsAndGroupForUpdatable(List<InboundDeliveryItem> inboundDeliveryItems) {
        return inboundDeliveryItems.stream()
                .map(it -> {
                    InboundDeliveryItem result = new InboundDeliveryItem();
                    result.setId(it.getId());
                    result.setLastVPLocationTypeCode(it.getLastVPLocationTypeCode());
                    result.setLastLocationAltKey(it.getLastLocationAltKey());
                    result.setLastEventName(it.getLastEventName());
                    result.setExecutionStatusCode(it.getExecutionStatusCode());
                    return result;
                }).collect(Collectors.toMap(InboundDeliveryItem::getId, identity()));
    }

    public static boolean isUpdatableEqual(PurchaseOrder left, PurchaseOrder right) {
        return isNullableEqual(left.getIsDelayed(), right.getIsDelayed())
                && getOrZero(left.getCompletionValue()).compareTo(getOrZero(right.getCompletionValue())) == 0
                && getOrZero(left.getCompletedAndLateValue()).compareTo(getOrZero(right.getCompletedAndLateValue())) == 0
                && getOrZero(left.getDelayedValue()).compareTo(getOrZero(right.getDelayedValue())) == 0;
    }

    public static boolean isUpdatableEqual(PurchaseOrderItem left, PurchaseOrderItem right) {
        return isNullableEqual(left.getIsDelayed(), right.getIsDelayed())
                && getOrZero(left.getCompletionValue()).compareTo(getOrZero(right.getCompletionValue())) == 0
                && getOrZero(left.getCompletedQuantity()).compareTo(getOrZero(right.getCompletedQuantity())) == 0
                && getOrZero(left.getCompletedAndLateValue()).compareTo(getOrZero(right.getCompletedAndLateValue())) == 0
                && getOrZero(left.getCompletedAndLateQuantity()).compareTo(getOrZero(right.getCompletedAndLateQuantity())) == 0
                && getOrZero(left.getDelayedQuantity()).compareTo(getOrZero(right.getDelayedQuantity())) == 0
                && getOrZero(left.getDelayedValue()).compareTo(getOrZero(right.getDelayedValue())) == 0;
    }

    public static boolean isUpdatableEqual(InboundDeliveryItem left, InboundDeliveryItem right) {
        return isNullableEqual(left.getLastVPLocationTypeCode(), right.getLastVPLocationTypeCode())
                && isNullableEqual(left.getLastLocationAltKey(), right.getLastLocationAltKey())
                && isNullableEqual(left.getLastEventName(), right.getLastEventName())
                && isNullableEqual(left.getExecutionStatusCode(), right.getExecutionStatusCode());
    }

    private static boolean isNullableEqual(Object left, Object right) {
        return Optional.ofNullable(left).equals(Optional.ofNullable(right));
    }

    private static BigDecimal getOrZero(BigDecimal bigDecimal) {
        return isNull(bigDecimal) ? BigDecimal.ZERO : bigDecimal;
    }
}
