package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl;

import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.LocationService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.PlannedEventService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.TimeTrackingService;
import com.sap.gtt.v2.sample.pof.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.pof.utils.TimelineEventConverter;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.sap.gtt.v2.sample.pof.constant.EventStatus.*;
import static com.sap.gtt.v2.sample.pof.utils.POFUtils.getDateTimeLong;
import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;
import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;

@Service
public class TimeTrackingServiceImpl implements TimeTrackingService {
    private PlannedEventService plannedEventService;
    private TimelineEventConverter timelineEventConverter;
    private ProcessEventDirectoryService processEventDirectoryService;
    private LocationService locationService;

    public TimeTrackingServiceImpl(PlannedEventService plannedEventService, TimelineEventConverter timelineEventConverter, ProcessEventDirectoryService processEventDirectoryService, LocationService locationService) {
        this.plannedEventService = plannedEventService;
        this.timelineEventConverter = timelineEventConverter;
        this.processEventDirectoryService = processEventDirectoryService;
        this.locationService = locationService;
    }

    @Override
    public List<TimelineEvent> getByDeliveryItemId(@NotNull final String deliveryItemId) {
        List<PlannedEvent> plannedEvents = plannedEventService.getAllByDeliveryItemId(deliveryItemId);
        List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getAllByDeliveryItemId(deliveryItemId);
        List<TimelineEvent> plannedTimelineEvents = getUnreportedPlannedTimelineEvents(plannedEvents);
        List<TimelineEvent> unplannedTimelineEvent = getUnplannedTimelineEvents(processEventDirectories);
        List<TimelineEvent> reportedPlannedTimelineEvent = getReportedPlannedEvent(plannedEvents,processEventDirectories);
        List<TimelineEvent> actualTimelineEvents = Stream.concat(unplannedTimelineEvent.stream(), reportedPlannedTimelineEvent.stream())
                .collect(Collectors.toList());
        actualTimelineEvents = sortActualTimelineEvents(actualTimelineEvents);
        List<TimelineEvent> timelineEvents = Stream.concat(plannedTimelineEvents.stream(), actualTimelineEvents.stream())
                .collect(Collectors.toList());
        locationService.fillLocations(timelineEvents);
        return timelineEvents;
    }


    private List<TimelineEvent> getUnreportedPlannedTimelineEvents(final List<PlannedEvent> plannedEvents) {
        List<PlannedEvent> sortedPlannedEvents = sortPlannedEvents(plannedEvents);
        return timelineEventConverter.toPlannedEvents(sortedPlannedEvents);
    }

    private List<PlannedEvent> sortPlannedEvents(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .filter(plannedEvent -> isUnreportedPlannedEvent(plannedEvent.getEventStatusCode()))
                .sorted(getPlannedEventComparator())
                .collect(toList());
    }

    private Comparator<PlannedEvent> getPlannedEventComparator() {
        return comparing(PlannedEvent::getPlannedBusinessTimestamp, nullsFirst(Long::compareTo))
                .thenComparing(PlannedEvent::getPayloadSequence, nullsFirst(Integer::compareTo))
                .reversed();
    }

    private boolean isUnreportedPlannedEvent(final String eventStatusCode) {
        return PLANNED.name().equals(eventStatusCode)
                || OVERDUE.name().equals(eventStatusCode)
                || DELAYED.name().equals(eventStatusCode);
    }

    private boolean isReportedPlannedEvent(final String eventStatusCode) {
        return LATE_REPORTED.name().equals(eventStatusCode)
                || REPORTED.name().equals(eventStatusCode)
                || EARLY_REPORTED.name().equals(eventStatusCode);
    }

    private List<TimelineEvent> getUnplannedTimelineEvents(List<ProcessEventDirectory> processEventDirectories) {
        List<ProcessEventDirectory> whitelistForTimelineEvents =
                ProcessEventDirectoryUtils.filterForUnplannedEvent(processEventDirectories);
        return timelineEventConverter.toUnplannedEvents(whitelistForTimelineEvents);
    }

    private List<TimelineEvent> getReportedPlannedEvent(List<PlannedEvent> plannedEvents , List<ProcessEventDirectory> processEventDirectories) {
        List<PlannedEvent> reportedPlannedEventList = plannedEvents.stream()
                .filter(plannedEvent -> isReportedPlannedEvent(plannedEvent.getEventStatusCode()))
                .collect(toList());
        Map<PlannedEvent,List<ProcessEventDirectory>> reportedPlannedEventMap = processEventDirectories.stream()
                .filter(processEventDirectory -> !isNull(processEventDirectory.getPlannedEvent()))
                .collect(Collectors.groupingBy(ProcessEventDirectory::getPlannedEvent));
        return timelineEventConverter.toReportedPlannedEvents(reportedPlannedEventList,reportedPlannedEventMap);
    }

    private List<TimelineEvent> sortActualTimelineEvents(List<TimelineEvent> actualEvents) {
        return actualEvents.stream()
                .sorted(getTimelineEventComparator())
                .collect(toList());
    }

    private Comparator<TimelineEvent> getTimelineEventComparator() {
        return comparing((TimelineEvent timelineEvent) ->
                getDateTimeLong(timelineEvent.getActualBusinessTimestamp()), nullsFirst(Long::compareTo)).reversed();
    }
}
