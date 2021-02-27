package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.DELAYED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.OVERDUE;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.PLANNED;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeLong;
import static java.util.Collections.emptyList;
import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import com.sap.gtt.v2.sample.sst.rest.model.converter.TimelineEventConverter;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * {@link TimelineEventAbstractService} is an abstract service which operates
 * on {@link TimelineEvent} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Service
public abstract class TimelineEventAbstractService {

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private EventHistoryService eventHistoryService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private TimelineEventConverter timelineEventConverter;

    /**
     * Retrieves {@link TimelineEvent} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of {@link TimelineEvent} entities
     */
    public abstract List<TimelineEvent> getByTrackedProcessId(@NotNull final String trackedProcessId);

    /**
     * Retrieves planned {@link TimelineEvent} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of planned {@link TimelineEvent} entities
     */
    protected List<TimelineEvent> getPlannedTimelineEvents(@NotNull final String trackedProcessId) {
        final List<PlannedEvent> plannedEvents = plannedEventService.getAllByTrackedProcessId(trackedProcessId);
        final List<PlannedEvent> sortedPlannedEvents = sortPlannedEvents(plannedEvents);
        return timelineEventConverter.fromPlannedEvents(sortedPlannedEvents);
    }

    /**
     * Retrieves actual {@link TimelineEvent} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of actual {@link TimelineEvent} entities
     */
    protected List<TimelineEvent> getActualTimelineEvents(@NotNull final String trackedProcessId) {
        final List<ProcessEventDirectory> processEventDirectories =
                processEventDirectoryService.getByTrackedProcessId(trackedProcessId);
        final List<TimelineEvent> actualEvents = retrieveActualEvents(processEventDirectories);
        return sortActualEvents(actualEvents);
    }

    private List<TimelineEvent> retrieveActualEvents(final List<ProcessEventDirectory> processEventDirectories) {
        final List<ProcessEventDirectory> eventsInWhitelist =
                ProcessEventDirectoryUtils.filterByWhitelistForTimelineEvents(processEventDirectories);
        final List<ProcessEventDirectory> actualEventsWithoutPlannedEvent =
                processEventDirectoryService.getWithoutPlannedEvent(eventsInWhitelist);
        final Map<ProcessEventDirectory, List<ProcessEventDirectory>> actualEventsByPlannedEvents =
                processEventDirectoryService.getActualEventsByPlannedEvents(eventsInWhitelist);
        final Map<ProcessEventDirectory, List<EventHistory>> plannedEventsWithHistory =
                eventHistoryService.getEventHistoryForPlannedEvents(actualEventsByPlannedEvents);
        final Map<ProcessEventDirectory, List<EventHistory>> actualEvents =
                mergeActualEvents(actualEventsWithoutPlannedEvent, plannedEventsWithHistory);
        return timelineEventConverter.fromActualEvents(actualEvents);
    }

    private Map<ProcessEventDirectory, List<EventHistory>> mergeActualEvents(
            final List<ProcessEventDirectory> actualEventsWithoutPlannedEvent,
            final Map<ProcessEventDirectory, List<EventHistory>> plannedEventsWithHistory) {
        final Map<ProcessEventDirectory, List<EventHistory>> actualEvents =
                actualEventsWithoutPlannedEvent
                        .stream()
                        .collect(toMap(Function.identity(), it -> emptyList()));

        plannedEventsWithHistory.keySet()
                .forEach(key -> actualEvents.put(key, plannedEventsWithHistory.get(key)));

        return actualEvents;
    }

    private List<TimelineEvent> sortActualEvents(final List<TimelineEvent> actualEvents) {
        return actualEvents.stream()
                .sorted(getTimelineEventComparator())
                .collect(toList());
    }

    private Comparator<TimelineEvent> getTimelineEventComparator() {
        return comparing((TimelineEvent timelineEvent) ->
                getDateTimeLong(timelineEvent.getActualTechnicalTimestamp()), nullsFirst(Long::compareTo)).reversed();
    }

    private List<PlannedEvent> sortPlannedEvents(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .filter(plannedEvent -> isSuitableEventStatus(plannedEvent.getEventStatusCode()))
                .sorted(getPlannedEventComparator())
                .collect(toList());
    }

    private Comparator<PlannedEvent> getPlannedEventComparator() {
        return comparing(PlannedEvent::getPlannedBusinessTimestamp, nullsFirst(Long::compareTo))
                .thenComparing(PlannedEvent::getPayloadSequence, nullsFirst(Integer::compareTo))
                .reversed();
    }

    private boolean isSuitableEventStatus(final String eventStatusCode) {
        return PLANNED.name().equals(eventStatusCode)
                || OVERDUE.name().equals(eventStatusCode)
                || DELAYED.name().equals(eventStatusCode);
    }
}
