package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventStatus.UNPLANNED;
import static java.util.Objects.nonNull;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link CurrentLocationConverter} is a converter which converts provided entities to {@link CurrentLocation}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class CurrentLocationConverter {

    @Autowired
    private EventService eventService;

    /**
     * Converts provided {@link ActualSpot} entity to {@link CurrentLocation} entity.
     *
     * @param actualSpot - {@link ActualSpot} entity to be converted
     * @return {@link CurrentLocation} entity
     */
    public CurrentLocation fromActualSpot(@NotNull final ActualSpot actualSpot) {
        final Event event = actualSpot.getEvent();
        final PlannedEvent plannedEvent = actualSpot.getPlannedEvent();
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventId(event.getId());
        currentLocation.setAltKey(event.getAltKey());
        currentLocation.setEventType(event.getEventType());
        currentLocation.setLongitude(actualSpot.getLongitude());
        currentLocation.setLatitude(actualSpot.getLatitude());
        currentLocation.setPlannedEvent(plannedEvent);
        fillEventStatusCode(currentLocation, plannedEvent);
        fillRefPlannedEventInformation(currentLocation, event);
        return currentLocation;
    }

    private void fillEventStatusCode(final CurrentLocation currentLocation, final PlannedEvent plannedEvent) {
        final String eventStatus = nonNull(plannedEvent)
                ? plannedEvent.getEventStatusCode()
                : UNPLANNED.name();
        currentLocation.setEventStatusCode(eventStatus);
    }

    private void fillRefPlannedEventInformation(final CurrentLocation currentLocation, final Event actualEvent) {
        final String actualEventId = actualEvent.getId().toString();
        final String actualEventType = actualEvent.getEventType();
        final Optional<Event> eventOpt = eventService.getById(actualEventId, actualEventType);
        if (eventOpt.isPresent()) {
            final Event event = eventOpt.get();
            currentLocation.setRefPlannedEventType(event.getRefPlannedEventType());
            currentLocation.setRefPlannedEventMatchKey(event.getRefPlannedEventMatchKey());
            currentLocation.setRefPlannedEventLocationAltKey(event.getRefPlannedEventLocationAltKey());
            currentLocation.setEstimatedArrival(event.getEstimatedArrival());
        }
    }
}
