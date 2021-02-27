package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.EventStatus;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.EventStatusService;
import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * {@link EventsByStatusAbstractService} is an abstract service which operates on {@link EventsByStatus} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Service
public abstract class EventsByStatusAbstractService {

    private static final String EVENT_STATUS_REPORTED = "REPORTED";
    private static final String EVENT_STATUS_EARLY_REPORTED = "EARLY_REPORTED";
    private static final String EVENT_STATUS_LATE_REPORTED = "LATE_REPORTED";

    @Autowired
    private EventStatusService eventStatusService;

    /**
     * Returns {@link EventsByStatus} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of {@link EventsByStatus} entities
     */
    public abstract List<EventsByStatus> getByTrackedProcessId(@NotNull final String trackedProcessId);

    /**
     * Calculates events amount by statuses.
     *
     * @param eventsByStatuses - map of events by statuses
     * @param plannedEvents    - {@link PlannedEvent} of tracked process
     * @return calculated list {@link EventsByStatus}
     */
    protected List<EventsByStatus> getCalculatedEventsAmountByStatuses(
            @NotNull final Map<String, Integer> eventsByStatuses, @NotNull final List<PlannedEvent> plannedEvents) {
        calculateAmountAndUpdateEventsByStatuses(eventsByStatuses, plannedEvents);
        return eventsByStatuses.keySet()
                .stream()
                .map(eventStatus -> new EventsByStatus(eventStatus, eventsByStatuses.get(eventStatus)))
                .collect(toList());
    }

    /**
     * Retrieves events by status codes.
     *
     * @return map of events by status codes
     */
    protected Map<String, Integer> collectEventsByStatusCodes() {
        final List<EventStatus> eventStatuses = eventStatusService.getAll();
        final Map<String, Integer> eventsByStatuses = new HashMap<>();
        for (EventStatus eventStatus : eventStatuses) {
            String eventStatusCode = eventStatus.getCode();
            if (EVENT_STATUS_EARLY_REPORTED.equals(eventStatusCode) || EVENT_STATUS_LATE_REPORTED.equals(eventStatusCode)) {
                eventStatusCode = EVENT_STATUS_REPORTED;
            }
            eventsByStatuses.put(eventStatusCode, 0);
        }
        return eventsByStatuses;
    }

    private void calculateAmountAndUpdateEventsByStatuses(
            final Map<String, Integer> eventsByStatuses, final List<PlannedEvent> plannedEvents) {
        for (PlannedEvent plannedEvent : plannedEvents) {
            String eventStatusCode = plannedEvent.getEventStatusCode();
            if (EVENT_STATUS_EARLY_REPORTED.equals(eventStatusCode) || EVENT_STATUS_LATE_REPORTED.equals(eventStatusCode)) {
                eventStatusCode = EVENT_STATUS_REPORTED;
            }
            final Integer currentAmount = eventsByStatuses.getOrDefault(eventStatusCode, 0);
            eventsByStatuses.put(eventStatusCode, currentAmount + 1);
        }
    }
}
