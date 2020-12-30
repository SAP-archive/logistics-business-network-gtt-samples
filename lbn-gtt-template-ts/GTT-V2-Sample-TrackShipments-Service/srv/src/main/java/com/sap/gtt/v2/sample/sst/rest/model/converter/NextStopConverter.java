package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType.ARRIVAL;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;

import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link NextStopConverter} is a converter which converts provided entities to {@link NextStop}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class NextStopConverter {

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private LocationService locationService;

    /**
     * Converts provided {@link EstimatedArrival} entity to {@link NextStop} entity.
     *
     * @param estimatedArrival - {@link EstimatedArrival} entity to be converted
     * @param shipmentId       - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return {@link NextStop} entity
     */
    public NextStop fromEstimatedArrival(
            @NotNull final EstimatedArrival estimatedArrival, @NotNull final String shipmentId) {
        final NextStop nextStop = new NextStop();
        final String stopId = estimatedArrival.getStopId();
        final Optional<PlannedEvent> plannedEventOpt = getRelatedPlannedEvent(shipmentId, stopId);
        nextStop.setEstimatedArrivalTime(getDateTimeString(estimatedArrival.getEstimatedArrivalTime()));
        plannedEventOpt.ifPresent(plannedEvent -> fillPlannedBusinessTimestamp(nextStop, plannedEvent));
        plannedEventOpt.ifPresent(plannedEvent -> fillLocation(nextStop, plannedEvent));
        return nextStop;
    }

    private void fillLocation(final NextStop nextStop, final PlannedEvent plannedEvent) {
        final String locationAltKey = plannedEvent.getLocationAltKey();
        final Optional<Location> locationOpt = locationService.getByAltKey(locationAltKey);
        locationOpt.ifPresent(nextStop::setLocation);
    }

    private void fillPlannedBusinessTimestamp(final NextStop nextStop, final PlannedEvent plannedEvent) {
        final Long plannedBusinessTimestamp = plannedEvent.getPlannedBusinessTimestamp();
        nextStop.setPlannedBusinessTimestamp(getDateTimeString(plannedBusinessTimestamp));
    }

    private Optional<PlannedEvent> getRelatedPlannedEvent(final String shipmentId, final String stopId) {
        final List<PlannedEvent> plannedEvents = plannedEventService.getAllByShipmentId(shipmentId);
        return plannedEvents.stream()
                .filter(plannedEvent -> isArrivalEventType(plannedEvent.getEventType()))
                .filter(plannedEvent -> plannedEvent.getEventMatchKey().equals(stopId))
                .findFirst();
    }

    private boolean isArrivalEventType(final String eventType) {
        return ARRIVAL.getValue().equals(getEventTypeShortName(eventType));
    }
}
