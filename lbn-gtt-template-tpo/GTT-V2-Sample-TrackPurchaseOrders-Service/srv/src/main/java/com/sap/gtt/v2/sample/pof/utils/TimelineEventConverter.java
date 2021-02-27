package com.sap.gtt.v2.sample.pof.utils;

import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;
import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.EventHistory;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

@Validated @Component public class TimelineEventConverter {
    public static final String UNPLANNED = "UNPLANNED";

    public List<TimelineEvent> toPlannedEvents(@NotNull final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream().map(this::convertPlannedEvent).collect(toList());
    }

    private TimelineEvent convertPlannedEvent(final PlannedEvent plannedEvent) {
        final TimelineEvent timelineEvent = new TimelineEvent();
        timelineEvent.setEventTypeFullName(plannedEvent.getEventType());
        timelineEvent.setEventStatusCode(plannedEvent.getEventStatusCode());
        timelineEvent.setPlannedBusinessTimestamp(plannedEvent.getPlannedBusinessTimestamp());
        timelineEvent.setLocationAltKey(plannedEvent.getLocationAltKey());
        timelineEvent.setPlannedEventId(plannedEvent.getId());
        timelineEvent.setEventMatchKey(plannedEvent.getEventMatchKey());
        return timelineEvent;
    }

    public List<TimelineEvent> toUnplannedEvents(@NotNull List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream().map(this::convertUnPlannedEvent).collect(toList());
    }

    private TimelineEvent convertUnPlannedEvent(ProcessEventDirectory processEventDirectory) {
        final TimelineEvent timelineEvent = new TimelineEvent();
        Event event = processEventDirectory.getEvent();
        timelineEvent.setEventTypeFullName(event.getEventType());
        timelineEvent.setEventStatusCode(UNPLANNED);
        timelineEvent.setActualEventId(event.getId());
        timelineEvent.setLocationAltKey(event.getLocationAltKey());
        timelineEvent.setActualBusinessTimestamp(event.getActualBusinessTimestamp());
        timelineEvent.setEventMatchKey(event.getEventMatchKey());
        return timelineEvent;
    }

    public List<TimelineEvent> toReportedPlannedEvents(@NotNull List<PlannedEvent> plannedEvents,
        @NotNull Map<PlannedEvent, List<ProcessEventDirectory>> reportedPlannedEventMap) {
        return plannedEvents.stream().
            map(plannedEvent -> convertReportedPlanned(plannedEvent, reportedPlannedEventMap.get(plannedEvent)))
            .collect(toList());
    }

    private TimelineEvent convertReportedPlanned(PlannedEvent plannedEvent,
        List<ProcessEventDirectory> processEventDirectories) {
        TimelineEvent timelineEvent = new TimelineEvent();
        timelineEvent.setEventTypeFullName(plannedEvent.getEventType());
        timelineEvent.setEventStatusCode(plannedEvent.getEventStatusCode());
        timelineEvent.setPlannedBusinessTimestamp(plannedEvent.getPlannedBusinessTimestamp());
        timelineEvent.setLocationAltKey(plannedEvent.getLocationAltKey());
        timelineEvent.setPlannedEventId(plannedEvent.getId());
        timelineEvent.setEventMatchKey(plannedEvent.getEventMatchKey());
        List<EventHistory> eventHistory = toEventHistory(processEventDirectories);
        timelineEvent.setEventHistory(eventHistory);
        Event event = plannedEvent.getLastProcessEventDirectory().getEvent();
        timelineEvent.setActualEventId(event.getId());
        timelineEvent.setActualBusinessTimestamp(event.getActualBusinessTimestamp());
        return timelineEvent;
    }

    private List<EventHistory> toEventHistory(@NotNull List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream().filter(
            processEventDirectory -> !isNull(processEventDirectory.getPlannedEvent())
                && !isNull(processEventDirectory.getEvent())
                && StringUtils.equals(processEventDirectory.getPlannedEvent().getEventType(),
                    processEventDirectory.getEvent().getEventType())).sorted(getProcessEventDirectoryComparator())
            .map(this::convertEventHistory).collect(toList());
    }

    private EventHistory convertEventHistory(@NotNull ProcessEventDirectory processEventDirectory) {
        EventHistory eventHistory = new EventHistory();
        Event event = processEventDirectory.getEvent();
        eventHistory.setId(processEventDirectory.getEventId());
        eventHistory.setAltKey(event.getAltKey());
        eventHistory.setActualBusinessTimestamp(event.getActualBusinessTimestamp());
        eventHistory.setActualBusinessTimeZone(event.getActualBusinessTimeZone());
        eventHistory.setLongitude(event.getLongitude());
        eventHistory.setLatitude(event.getLatitude());
        eventHistory.setLocationAltKey(event.getLocationAltKey());
        eventHistory.setReportedBy(event.getReportedBy());
        eventHistory.setSenderPartyId(event.getSenderPartyId());
        eventHistory.setEventMatchKey(event.getEventMatchKey());
        eventHistory.setEventType(event.getEventType());
        eventHistory.setActualTechTimestamp(event.getActualTechnicalTimestamp());
        return eventHistory;
    }

    private Comparator<ProcessEventDirectory> getProcessEventDirectoryComparator() {
        return comparing((ProcessEventDirectory processEventDirectory) -> processEventDirectory.getEvent()
            .getActualTechnicalTimestamp(), nullsFirst(Long::compareTo)).reversed();
    }

}
