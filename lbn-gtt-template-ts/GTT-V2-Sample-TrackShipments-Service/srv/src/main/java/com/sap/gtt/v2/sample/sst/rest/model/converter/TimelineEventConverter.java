package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.DELAYED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.UNPLANNED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.CHECK_IN;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.EXCEPTIONAL_EVENT;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.OTHER_EVENT;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.OUT_FOR_DELIVERY;
import static com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils.retrieveLastProcessEventDirectory;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;
import static java.util.Collections.emptyList;
import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.LocationType;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link TimelineEventConverter} is a converter which converts provided entities to {@link TimelineEvent}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class TimelineEventConverter {

    @Autowired
    private LocationService locationService;

    @Autowired
    private EventService eventService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    /**
     * Converts provided {@link PlannedEvent} entities to {@link TimelineEvent} entities.
     *
     * @param plannedEvents - {@link PlannedEvent} entity to be converted
     * @return list of {@link TimelineEvent} entities
     */
    public List<TimelineEvent> fromPlannedEvents(@NotNull final List<PlannedEvent> plannedEvents) {
        final List<ProcessEventDirectory> lastProcessEventDirectories = getLastProcessEventDirectoriesIfNeeded(plannedEvents);
        final List<TimelineEvent> convertedPlannedEvents = plannedEvents.stream()
                .map(plannedEvent -> convertPlannedEvent(plannedEvent, lastProcessEventDirectories))
                .collect(toList());
        fillLocationInformationForPlannedEvents(convertedPlannedEvents);
        return convertedPlannedEvents;
    }

    /**
     * Converts provided {@link Map} of actual events to {@link TimelineEvent} entities.
     *
     * @param processEventDirectories - map of {@link ProcessEventDirectory} with theirs {@link EventHistory}
     * @return list of {@link TimelineEvent} entities
     */
    public List<TimelineEvent> fromActualEvents(
            @NotNull final Map<ProcessEventDirectory, List<EventHistory>> processEventDirectories) {
        final List<TimelineEvent> convertedActualEvents = processEventDirectories.keySet().stream()
                .map(actualEvent -> convertActualEvent(actualEvent, processEventDirectories.get(actualEvent)))
                .collect(toList());
        fillLocationInformationForActualEvents(convertedActualEvents);
        return convertedActualEvents;
    }

    private TimelineEvent convertActualEvent(
            final ProcessEventDirectory processEventDirectory, final List<EventHistory> eventHistory) {
        return isNull(processEventDirectory.getPlannedEvent())
                ? toActualEventWithoutPlannedEvent(processEventDirectory)
                : toActualEventWithPlannedEvent(processEventDirectory, eventHistory);
    }

    private TimelineEvent convertPlannedEvent(
            final PlannedEvent plannedEvent, final List<ProcessEventDirectory> lastProcessEventDirectories) {
        final TimelineEvent timelineEvent = new TimelineEvent();
        timelineEvent.setEventTypeFullName(plannedEvent.getEventType());
        timelineEvent.setEventType(getEventTypeShortName(plannedEvent.getEventType()));
        timelineEvent.setEventStatusCode(plannedEvent.getEventStatusCode());
        timelineEvent.setPlannedBusinessTimestamp(getDateTimeString(plannedEvent.getPlannedBusinessTimestamp()));
        timelineEvent.setPlannedEventLocationAltKey(plannedEvent.getLocationAltKey());
        timelineEvent.setPlannedEventId(plannedEvent.getId());
        timelineEvent.setEventMatchKey(plannedEvent.getEventMatchKey());
        timelineEvent.setPlannedEvent(plannedEvent);
        setEventReasonTextIfNeeded(timelineEvent, plannedEvent, lastProcessEventDirectories);
        return timelineEvent;
    }

    private TimelineEvent toActualEventWithoutPlannedEvent(final ProcessEventDirectory processEventDirectory) {
        final TimelineEvent timelineEvent = new TimelineEvent();
        final Event event = processEventDirectory.getEvent();
        timelineEvent.setEventTypeFullName(event.getEventType());
        timelineEvent.setEventType(getEventTypeShortName(event.getEventType()));
        timelineEvent.setEventStatusCode(UNPLANNED.name());
        timelineEvent.setActualBusinessTimestamp(getDateTimeString(event.getActualBusinessTimestamp()));
        timelineEvent.setActualTechnicalTimestamp(getDateTimeString(event.getActualTechnicalTimestamp()));
        timelineEvent.setActualEventId(event.getId());
        timelineEvent.setEventMatchKey(event.getEventMatchKey());
        timelineEvent.setActualEventLocationAltKey(event.getLocationAltKey());
        return timelineEvent;
    }

    private TimelineEvent toActualEventWithPlannedEvent(
            final ProcessEventDirectory processEventDirectory, final List<EventHistory> eventHistory) {
        final TimelineEvent timelineEvent = new TimelineEvent();
        final PlannedEvent plannedEvent = processEventDirectory.getPlannedEvent();
        final Event event = processEventDirectory.getEvent();
        timelineEvent.setEventTypeFullName(plannedEvent.getEventType());
        timelineEvent.setEventType(getEventTypeShortName(plannedEvent.getEventType()));
        timelineEvent.setEventStatusCode(plannedEvent.getEventStatusCode());
        timelineEvent.setPlannedBusinessTimestamp(getDateTimeString(plannedEvent.getPlannedBusinessTimestamp()));
        timelineEvent.setPlannedEventLocationAltKey(plannedEvent.getLocationAltKey());
        timelineEvent.setActualEventLocationAltKey(event.getLocationAltKey());
        timelineEvent.setActualEventId(event.getId());
        timelineEvent.setPlannedEventId(plannedEvent.getId());
        timelineEvent.setEventMatchKey(event.getEventMatchKey());
        timelineEvent.setEventHistory(eventHistory);
        timelineEvent.setActualBusinessTimestamp(getDateTimeString(event.getActualBusinessTimestamp()));
        timelineEvent.setActualTechnicalTimestamp(getDateTimeString(event.getActualTechnicalTimestamp()));
        timelineEvent.setPlannedEvent(plannedEvent);
        return timelineEvent;
    }

    private List<ProcessEventDirectory> getLastProcessEventDirectoriesIfNeeded(final List<PlannedEvent> plannedEvents) {
        final List<UUID> lastProcessEventDirectoriesIds = getLastProcessEventDirectoriesIds(plannedEvents);
        return lastProcessEventDirectoriesIds.isEmpty()
                ? emptyList()
                : processEventDirectoryService.getByIds(lastProcessEventDirectoriesIds);
    }

    private List<UUID> getLastProcessEventDirectoriesIds(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .filter(plannedEvent -> DELAYED.name().equals(plannedEvent.getEventStatusCode()))
                .map(PlannedEvent::getLastProcessEventDirectoryId)
                .filter(Objects::nonNull)
                .collect(toList());
    }

    private void setEventReasonTextIfNeeded(
            final TimelineEvent timelineEvent,
            final PlannedEvent plannedEvent,
            final List<ProcessEventDirectory> lastProcessEventDirectories) {
        final String plannedEventStatusCode = plannedEvent.getEventStatusCode();
        if (DELAYED.name().equals(plannedEventStatusCode)) {
            final UUID lastProcessEventDirectoryId = plannedEvent.getLastProcessEventDirectoryId();
            final Optional<ProcessEventDirectory> lastProcessEventDirectoryOpt =
                    retrieveLastProcessEventDirectory(lastProcessEventDirectoryId, lastProcessEventDirectories);
            lastProcessEventDirectoryOpt.ifPresent(lastProcessEventDirectory -> {
                final Event event = lastProcessEventDirectory.getEvent();
                timelineEvent.setEventReasonCode(event.getEventReasonCode());
                timelineEvent.setEventReasonText(event.getEventReasonText());
            });
        }
    }

    private void fillLocationInformationForPlannedEvents(final List<TimelineEvent> timelineEvents) {
        final List<Location> locations = !timelineEvents.isEmpty() ? locationService.getAll() : emptyList();
        fillPlannedEventLocationInformationIfNeeded(timelineEvents, locations);
    }

    private void fillLocationInformationForActualEvents(final List<TimelineEvent> timelineEvents) {
        final List<Location> locations = !timelineEvents.isEmpty() ? locationService.getAll() : emptyList();
        fillPlannedEventLocationInformationIfNeeded(timelineEvents, locations);
        fillActualEventLocationInformationIfNeeded(timelineEvents, locations);
        fillLocationTypeIfNeeded(timelineEvents);
    }

    private void fillPlannedEventLocationInformationIfNeeded(final List<TimelineEvent> timelineEvents, final List<Location> locations) {
        timelineEvents.stream()
                .filter(timelineEvent -> nonNull(timelineEvent.getPlannedEventLocationAltKey()))
                .forEach(timelineEvent -> {
                    final String locationAltKey = timelineEvent.getPlannedEventLocationAltKey();
                    fillLocation(timelineEvent, locationAltKey, locations);
                });
    }

    private void fillActualEventLocationInformationIfNeeded(final List<TimelineEvent> timelineEvents, final List<Location> locations) {
        timelineEvents.stream()
                .filter(timelineEvent -> isNull(timelineEvent.getLocation()))
                .filter(timelineEvent -> nonNull(timelineEvent.getActualEventLocationAltKey()))
                .forEach(timelineEvent -> {
                    final String locationAltKey = timelineEvent.getActualEventLocationAltKey();
                    fillLocation(timelineEvent, locationAltKey, locations);
                });
    }

    private void fillLocation(final TimelineEvent timelineEvent, final String locationAltKey, final List<Location> locations) {
        final Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
        locationOpt.ifPresent(timelineEvent::setLocation);
        timelineEvent.setLocationAltKey(locationAltKey);
    }

    private void fillLocationTypeIfNeeded(final List<TimelineEvent> timelineEvents) {
        final List<TimelineEvent> filteredTimelineEvents = filterOutTimelineEventsByEventType(timelineEvents);
        final Map<String, List<TimelineEvent>> timelineEventsByEventTypes = groupTimelineEventsByEventTypes(filteredTimelineEvents);
        timelineEventsByEventTypes.keySet()
                .forEach(eventType -> fillLocationTypeByEventType(eventType, timelineEventsByEventTypes.get(eventType)));
    }

    private void fillLocationTypeByEventType(final String eventType, final List<TimelineEvent> timelineEvents) {
        final List<UUID> actualEventIds = getActualEventIdsFromTimelineEvents(timelineEvents);
        final List<Event> eventsByType = eventService.getByEventType(eventType, actualEventIds);
        timelineEvents.forEach(timelineEvent -> {
            final UUID actualEventId = timelineEvent.getActualEventId();
            final Optional<LocationType> locationTypeOpt = retrieveLocationTypeByEventId(eventsByType, actualEventId);
            locationTypeOpt.ifPresent(timelineEvent::setLocationType);
        });
    }

    private List<UUID> getActualEventIdsFromTimelineEvents(final List<TimelineEvent> timelineEvents) {
        return timelineEvents.stream()
                .map(TimelineEvent::getActualEventId)
                .collect(toList());
    }

    private Optional<LocationType> retrieveLocationTypeByEventId(final List<Event> eventsByType, final UUID eventId) {
        return eventsByType.stream()
                .filter(event -> event.getId().equals(eventId))
                .findFirst()
                .map(Event::getLocationType);
    }

    private List<TimelineEvent> filterOutTimelineEventsByEventType(final List<TimelineEvent> timelineEvents) {
        return timelineEvents.stream()
                .filter(timelineEvent -> isNull(timelineEvent.getLocation()))
                .filter(this::isSuitableEventTypeForLocationType)
                .collect(toList());
    }

    private Map<String, List<TimelineEvent>> groupTimelineEventsByEventTypes(final List<TimelineEvent> timelineEvents) {
        return timelineEvents.stream().collect(groupingBy(TimelineEvent::getEventType));
    }

    private boolean isSuitableEventTypeForLocationType(final TimelineEvent timelineEvent) {
        final String eventType = timelineEvent.getEventType();
        return !CHECK_IN.getValue().equals(eventType)
                && !OUT_FOR_DELIVERY.getValue().equals(eventType)
                && !OTHER_EVENT.getValue().equals(eventType)
                && !EXCEPTIONAL_EVENT.getValue().equals(eventType);
    }
}
