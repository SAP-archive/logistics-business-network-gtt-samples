package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventStatus.DELAYED;
import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventStatus.OVERDUE;
import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventStatus.PLANNED;
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
import java.util.stream.Stream;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class TimelineEventServiceImpl implements TimelineEventService {

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private EventHistoryService eventHistoryService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private TimelineEventConverter timelineEventConverter;

    @Override
    public List<TimelineEvent> getByShipmentId(@NotNull final String shipmentId) {
        final List<TimelineEvent> plannedTimelineEvents = getPlannedTimelineEvents(shipmentId);
        final List<TimelineEvent> actualTimelineEvents = getActualTimelineEvents(shipmentId);
        return Stream.concat(plannedTimelineEvents.stream(), actualTimelineEvents.stream()).collect(toList());
    }

    private List<TimelineEvent> getPlannedTimelineEvents(final String shipmentId) {
        final List<PlannedEvent> plannedEvents = plannedEventService.getAllByShipmentId(shipmentId);
        final List<PlannedEvent> sortedPlannedEvents = sortPlannedEvents(plannedEvents);
        return timelineEventConverter.fromPlannedEvents(sortedPlannedEvents);
    }

    private List<TimelineEvent> getActualTimelineEvents(final String shipmentId) {
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByShipmentId(shipmentId);
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
