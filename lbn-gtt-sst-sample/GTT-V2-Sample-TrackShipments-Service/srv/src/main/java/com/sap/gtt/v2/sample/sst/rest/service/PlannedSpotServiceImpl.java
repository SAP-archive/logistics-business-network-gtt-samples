package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType.DELAY;
import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType.LOCATION_UPDATE;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.isPlannedEventNotBeenReported;
import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.PlannedSpotConverter;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import javax.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Min Li
 */
@Validated
@Service
public class PlannedSpotServiceImpl implements PlannedSpotService {

    @Autowired
    private PlannedSpotConverter plannedSpotConverter;

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    @Override
    public List<PlannedSpot> getAll(@NotNull final List<PlannedEvent> plannedEvents) {
        return plannedSpotConverter.fromPlannedEvents(plannedEvents);
    }

    @Override
    public PlannedSpot findNextPlannedSpot(CurrentLocation currentLocation, List<PlannedSpot> plannedSpots, List<PlannedEvent> plannedEvents) {
        String eventType = currentLocation.getEventType();
        PlannedEvent plannedEventOfCurrentLocation = currentLocation.getPlannedEvent();
        if (isDelayOrLocationUpdateEvent(eventType)) {
            String refPlannedEventType = currentLocation.getRefPlannedEventType();
            String refPlannedEventMatchKey = currentLocation.getRefPlannedEventMatchKey();
            return findNextPlannedSpotForDelayOrLocationUpdateEvent(plannedSpots, refPlannedEventType, refPlannedEventMatchKey);
        } else if (nonNull(plannedEventOfCurrentLocation)) {
            return findNextPlannedSpotForActualEventWithPlannedCorrelation(plannedSpots, plannedEvents, plannedEventOfCurrentLocation);
        } else {
            return null;
        }
    }

    private boolean isDelayOrLocationUpdateEvent(final String eventType) {
        String eventTypeShortName = getEventTypeShortName(eventType);
        return DELAY.getValue().equals(eventTypeShortName)
                || LOCATION_UPDATE.getValue().equals(eventTypeShortName);
    }

    private PlannedSpot findNextPlannedSpotForDelayOrLocationUpdateEvent(
            final List<PlannedSpot> plannedSpots,
            final String refPlannedEventType,
            final String refPlannedEventMatchKey) {
        return isNull(refPlannedEventType) || isNull(refPlannedEventMatchKey)
                ? null
                : plannedSpots.stream()
                .filter(plannedSpot -> StringUtils.equals(refPlannedEventType, plannedSpot.getEventType())
                        && StringUtils.equals(refPlannedEventMatchKey, plannedSpot.getEventMatchKey()))
                .findFirst()
                .orElse(null);
    }

    private PlannedSpot findNextPlannedSpotForActualEventWithPlannedCorrelation(
            final List<PlannedSpot> plannedSpots,
            final List<PlannedEvent> plannedEvents,
            final PlannedEvent plannedEventOfCurrentLocation) {
        int index = IntStream.range(0, plannedEvents.size())
                .filter(i -> plannedEvents.get(i).getId().compareTo(plannedEventOfCurrentLocation.getId()) == 0)
                .findFirst()
                .orElse(-1);
        if (index != -1 && index < plannedEvents.size() - 1) {
            Optional<PlannedEvent> nextPlannedEvent = plannedEvents.subList(index + 1, plannedEvents.size()).stream()
                    .filter(plannedEvent -> coordinatesValidator.isValid(plannedEvent.getLongitude(), plannedEvent.getLatitude())
                            && isPlannedEventNotBeenReported(plannedEvent.getEventStatusCode()))
                    .findFirst();
            return nextPlannedEvent.flatMap(plannedEvent -> plannedSpots.stream()
                    .filter(plannedSpot -> plannedSpot.getEventId().compareTo(plannedEvent.getId()) == 0)
                    .findFirst())
                    .orElse(null);
        }
        return null;
    }

}
