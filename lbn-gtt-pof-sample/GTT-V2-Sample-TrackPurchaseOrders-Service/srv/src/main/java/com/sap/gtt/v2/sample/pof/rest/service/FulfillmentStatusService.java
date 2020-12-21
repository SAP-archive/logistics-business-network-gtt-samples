package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.model.EventStatus;
import com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem.FulfillmentStatus;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.apache.commons.lang3.StringUtils.EMPTY;

@Service
public class FulfillmentStatusService extends AbstractEventService {

    private static final String EVENT_STATUSES_URL = "/EventStatus";

    public FulfillmentStatusService(GTTCoreServiceClient gttCoreServiceClient) {
        super(gttCoreServiceClient);
    }

    public List<FulfillmentStatus> getFulfillmentStatus(UUID inboundDeliveryItemId) {
        Map<String, Integer> statusesCountMap = initEventStatusesMap();
        List<PlannedEvent> plannedEvents = queryAllEvents(PLANNED_EVENTS_URL_TEMPLATE, inboundDeliveryItemId, PlannedEvent.class);
        plannedEvents.stream()
                .map(PlannedEvent::getEventStatusCode)
                .map(this::normalizeToReported)
                .forEach(code -> statusesCountMap.computeIfPresent(code, (key, value) -> ++value));
        return statusesCountMap.entrySet().stream()
                .map(it -> new FulfillmentStatus(it.getKey(), it.getValue()))
                .collect(Collectors.toList());
    }

    private Map<String, Integer> initEventStatusesMap() {
        List<EventStatus> eventStatuses = queryAll(EVENT_STATUSES_URL, EventStatus.class);
        return eventStatuses.stream()
                .map(EventStatus::getCode)
                .map(this::normalizeToReported)
                .distinct()
                .collect(Collectors.toMap(Function.identity(), v -> 0));
    }

    private String normalizeToReported(String eventStatusCode) {
        return eventStatusCode.replaceAll("(LATE|EARLY)_", EMPTY);
    }
}
