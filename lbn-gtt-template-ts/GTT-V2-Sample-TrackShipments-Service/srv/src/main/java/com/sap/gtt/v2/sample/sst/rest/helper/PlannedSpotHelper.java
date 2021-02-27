package com.sap.gtt.v2.sample.sst.rest.helper;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.EARLY_REPORTED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.LATE_REPORTED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.REPORTED;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.DELAY;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.LOCATION_UPDATE;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static java.util.Collections.emptyList;
import static java.util.Objects.isNull;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.IntStream;
import javax.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * {@link PlannedSpotHelper} is a helper class which operates on {@link PlannedSpot} entities.
 *
 * @author Min Li
 */
@Validated
@Service
public class PlannedSpotHelper {

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    /**
     * Retrieves {@link PlannedSpot} entity.
     *
     * @param currentLocation - {@link CurrentLocation} entity
     * @param plannedSpots    - list of {@link PlannedSpot} entities
     * @param plannedEvents   - list of {@link PlannedEvent} entities
     * @return {@link PlannedSpot} entity wrapped in {@link Optional}
     */
    public Optional<PlannedSpot> findNextPlannedSpot(
            @NotNull final CurrentLocation currentLocation,
            @NotNull final List<PlannedSpot> plannedSpots,
            @NotNull final List<PlannedEvent> plannedEvents) {
        final String eventType = currentLocation.getEventType();
        return isDelayOrLocationUpdateEvent(eventType)
                ? findNextPlannedSpotForDelayOrLocationUpdateEvent(plannedSpots, plannedEvents, currentLocation)
                : findNextPlannedSpotBasedOnLastReportedEvent(plannedSpots, plannedEvents);
    }

    private boolean isDelayOrLocationUpdateEvent(final String eventType) {
        final String eventTypeShortName = getEventTypeShortName(eventType);
        return DELAY.getValue().equals(eventTypeShortName)
                || LOCATION_UPDATE.getValue().equals(eventTypeShortName);
    }

    private Optional<PlannedSpot> findNextPlannedSpotForDelayOrLocationUpdateEvent(
            final List<PlannedSpot> plannedSpots,
            final List<PlannedEvent> plannedEvents,
            final CurrentLocation currentLocation) {
        final Optional<PlannedSpot> plannedSpotOpt = findReferredPlannedSpot(plannedSpots, currentLocation);
        return plannedSpotOpt.isPresent()
                ? plannedSpotOpt
                : findNextPlannedSpotBasedOnLastReportedEvent(plannedSpots, plannedEvents);
    }

    private Optional<PlannedSpot> findReferredPlannedSpot(
            final List<PlannedSpot> plannedSpots, final CurrentLocation currentLocation) {
        final String refPlannedEventType = currentLocation.getRefPlannedEventType();
        final String refPlannedEventMatchKey = currentLocation.getRefPlannedEventMatchKey();
        return isNull(refPlannedEventType) || isNull(refPlannedEventMatchKey)
                ? Optional.empty()
                : plannedSpots.stream()
                .filter(plannedSpot -> hasEqualEvents(plannedSpot, refPlannedEventType, refPlannedEventMatchKey))
                .findFirst();
    }

    private boolean hasEqualEvents(
            final PlannedSpot plannedSpot, final String refPlannedEventType, final String refPlannedEventMatchKey) {
        final String plannedSpotEventType = plannedSpot.getEventType();
        final String plannedSpotEventMatchKey = plannedSpot.getEventMatchKey();
        return StringUtils.equals(refPlannedEventType, plannedSpotEventType)
                && StringUtils.equals(refPlannedEventMatchKey, plannedSpotEventMatchKey);
    }

    private Optional<PlannedSpot> findNextPlannedSpotBasedOnLastReportedEvent(
            final List<PlannedSpot> plannedSpots, final List<PlannedEvent> plannedEvents) {
        final List<PlannedEvent> plannedEventsSublist = getPlannedEventsSublist(plannedEvents);
        return plannedEventsSublist.stream()
                .filter(this::plannedEventHasValidCoordinates)
                .findFirst()
                .flatMap(plannedEvent -> mapPlannedEventToEqualPlannedSpot(plannedEvent, plannedSpots));
    }

    private List<PlannedEvent> getPlannedEventsSublist(final List<PlannedEvent> plannedEvents) {
        final int index = findLastReportedPlannedEventIndex(plannedEvents);
        return (index != -1) && (index < plannedEvents.size() - 1)
                ? plannedEvents.subList(index + 1, plannedEvents.size())
                : emptyList();
    }

    private int findLastReportedPlannedEventIndex(final List<PlannedEvent> plannedEvents) {
        return IntStream.range(0, plannedEvents.size())
                .filter(index -> isPlannedEventReported(plannedEvents.get(index)))
                .reduce((first, second) -> second)
                .orElse(-1);
    }

    private Optional<PlannedSpot> mapPlannedEventToEqualPlannedSpot(
            final PlannedEvent plannedEvent, final List<PlannedSpot> plannedSpots) {
        final UUID plannedEventId = plannedEvent.getId();
        return plannedSpots.stream()
                .filter(plannedSpot -> plannedSpot.getEventId().compareTo(plannedEventId) == 0)
                .findFirst();
    }

    private boolean isPlannedEventReported(final PlannedEvent plannedEvent) {
        final String plannedEventEventStatusCode = plannedEvent.getEventStatusCode();
        return REPORTED.name().equals(plannedEventEventStatusCode)
                || EARLY_REPORTED.name().equals(plannedEventEventStatusCode)
                || LATE_REPORTED.name().equals(plannedEventEventStatusCode);
    }

    private boolean plannedEventHasValidCoordinates(final PlannedEvent plannedEvent) {
        final BigDecimal longitude = plannedEvent.getLongitude();
        final BigDecimal latitude = plannedEvent.getLatitude();
        return coordinatesValidator.isValid(longitude, latitude);
    }
}
