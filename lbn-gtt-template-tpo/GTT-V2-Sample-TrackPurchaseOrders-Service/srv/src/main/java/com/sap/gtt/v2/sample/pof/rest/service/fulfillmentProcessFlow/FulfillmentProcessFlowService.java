package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.pof.rest.service.AbstractEventService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_CONFIRMED;
import static com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_CREATED;
import static com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_DELETED;
import static com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum.PURCHASE_ORDER_ITEM_RECEIPT;
import static java.lang.String.format;

@Service
public class FulfillmentProcessFlowService extends AbstractEventService {

    private static final String EXPAND_VALUE = "inboundDeliveryItems,plannedEvents";
    private static final String PURCHASE_ORDER_ITEM_URI_TEMPLATE = "/PurchaseOrderItem(guid'%s')";

    @Autowired
    private MilestonePopulateExecutor executor;

    public FulfillmentProcessFlow generateFulfillmentProcessFlow(UUID purchaseOrderItemId) {
        FulfillmentProcessFlow result = new FulfillmentProcessFlow();
        PurchaseOrderItem item = queryPurchaseOrderItem(purchaseOrderItemId);
        List<Lane> lanes = initLanes(item);
        populateLanes(lanes, item);
        result.setLanes(lanes);
        return result;
    }

    private PurchaseOrderItem queryPurchaseOrderItem(UUID purchaseOrderItemId) {
        String httpUrl = format(PURCHASE_ORDER_ITEM_URI_TEMPLATE, purchaseOrderItemId);
        String uri = UriComponentsBuilder.fromUriString(httpUrl).queryParam(EXPAND, EXPAND_VALUE).encode().toUriString();
        return gttCoreServiceClient.readEntity(uri, PurchaseOrderItem.class);
    }

    private List<Lane> initLanes(PurchaseOrderItem item) {
        AtomicInteger position = new AtomicInteger();
        return EnumSet.allOf(FulfillmentProcessMilestoneEnum.class).stream()
                .filter(getMilestonesPredicate(item))
                .sorted(Comparator.comparingLong(FulfillmentProcessMilestoneEnum::getOrder))
                .map(milestone -> {
                    Lane lane = new Lane();
                    lane.setId(String.valueOf(milestone.getOrder()));
                    lane.setName(milestone.getName());
                    lane.setPosition(position.getAndIncrement());
                    lane.setTotal(Optional.ofNullable(item.getOrderQuantity()).orElse(BigDecimal.ZERO));
                    return lane;
                }).collect(Collectors.toList());
    }

    private void populateLanes(List<Lane> lanes, PurchaseOrderItem purchaseOrderItem) {
        lanes.forEach(lane -> executor.execute(lane, purchaseOrderItem));
    }

    private Predicate<? super FulfillmentProcessMilestoneEnum> getMilestonesPredicate(PurchaseOrderItem item) {
        if (isDeletionLatestEvent(item.getId())) {
            return m -> PURCHASE_ORDER_ITEM_CREATED.equals(m) || PURCHASE_ORDER_ITEM_DELETED.equals(m);
        } else if (isDeliveryItemsPresent(item)) {
            Set<String> plannedEventTypes = getAllPlannedEventTypes(item.getId());
            return m -> PURCHASE_ORDER_ITEM_CONFIRMED.equals(m)
                    ? plannedEventTypes.contains(m.getEventType())
                    : !PURCHASE_ORDER_ITEM_RECEIPT.equals(m) && !PURCHASE_ORDER_ITEM_DELETED.equals(m);
        } else {
            Set<String> plannedEventTypes = getAllPlannedEventTypes(item.getId());
            return m -> PURCHASE_ORDER_ITEM_CREATED.equals(m) || plannedEventTypes.contains(m.getEventType());
        }
    }

    private boolean isDeliveryItemsPresent(PurchaseOrderItem purchaseOrderItem) {
        return purchaseOrderItem.getInboundDeliveryItems().stream()
                .anyMatch(Objects::nonNull);
    }

    private Set<String> getAllPlannedEventTypes(UUID purchaseOrderItemId) {
        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENTS_URL_TEMPLATE, purchaseOrderItemId, PlannedEvent.class);
        return plannedEvents.stream()
                .map(PlannedEvent::getEventType)
                .map(eventType -> eventType.substring(eventType.lastIndexOf('.') + 1))
                .collect(Collectors.toSet());
    }
}
