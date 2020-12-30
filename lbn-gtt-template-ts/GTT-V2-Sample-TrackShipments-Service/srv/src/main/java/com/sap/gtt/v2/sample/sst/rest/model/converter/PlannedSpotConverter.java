package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link PlannedSpotConverter} is a converter which converts provided entities to {@link PlannedSpot}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class PlannedSpotConverter {

    @Autowired
    private LocationService locationService;

    /**
     * Converts provided {@link PlannedEvent} entities to {@link PlannedSpot} entities.
     *
     * @param plannedEvents - list of {@link PlannedEvent} entities to be converted
     * @return list of {@link PlannedSpot} entities
     */
    public List<PlannedSpot> fromPlannedEvents(@NotNull final List<PlannedEvent> plannedEvents) {
        final List<Location> locations = locationService.getAll();
        return plannedEvents.stream()
                .map(plannedEvent -> convertToPlannedSpot(plannedEvent, locations))
                .collect(toList());
    }

    private PlannedSpot convertToPlannedSpot(final PlannedEvent plannedEvent, final List<Location> locations) {
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventId(plannedEvent.getId());
        plannedSpot.setEventType(plannedEvent.getEventType());
        plannedSpot.setEventMatchKey(plannedEvent.getEventMatchKey());
        plannedSpot.setLocationAltKey(plannedEvent.getLocationAltKey());
        plannedSpot.setEventStatusCode(plannedEvent.getEventStatusCode());
        plannedSpot.setPlannedBusinessTimestamp(getDateTimeString(plannedEvent.getPlannedBusinessTimestamp()));
        fillLocationInformation(locations, plannedEvent, plannedSpot);
        return plannedSpot;
    }

    private void fillLocationInformation(
            final List<Location> locations,
            final PlannedEvent plannedEvent,
            final PlannedSpot plannedSpot) {
        final String locationAltKey = plannedEvent.getLocationAltKey();
        final Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
        if (locationOpt.isPresent()) {
            final Location location = locationOpt.get();
            plannedSpot.setLatitude(location.getLatitude());
            plannedSpot.setLongitude(location.getLongitude());
            // Need to set coordinate value of plannedEvent for finding next planned event
            plannedEvent.setLatitude(location.getLatitude());
            plannedEvent.setLongitude(location.getLongitude());
        }
    }

}
