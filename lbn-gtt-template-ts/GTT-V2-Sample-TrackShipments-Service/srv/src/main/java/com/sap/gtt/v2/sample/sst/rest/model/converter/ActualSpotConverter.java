package com.sap.gtt.v2.sample.sst.rest.model.converter;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;

/**
 * {@link ActualSpotConverter} is a converter which converts provided entities to {@link ActualSpot}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class ActualSpotConverter {

    @Autowired
    private LocationService locationService;

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    /**
     * Converts provided list of {@link ProcessEventDirectory} entities to {@link ActualSpot} entities.
     *
     * @param processEventDirectories - list of {@link ProcessEventDirectory} to be converted
     * @return list of {@link ActualSpot}
     */
    public List<ActualSpot> fromProcessEventDirectories(@NotNull final List<ProcessEventDirectory> processEventDirectories) {
        final List<Location> locations = locationService.getAll();
        return processEventDirectories.stream()
                .map(processEventDirectory -> convertToActualSpot(processEventDirectory, locations))
                .collect(toList());
    }

    private ActualSpot convertToActualSpot(final ProcessEventDirectory processEventDirectory, final List<Location> locations) {
        final Event event = processEventDirectory.getEvent();
        final PlannedEvent plannedEvent = processEventDirectory.getPlannedEvent();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setAltKey(event.getAltKey());
        actualSpot.setEventId(event.getId());
        actualSpot.setEventType(event.getEventType());
        actualSpot.setActualBusinessTimestamp(getDateTimeString(event.getActualBusinessTimestamp()));
        actualSpot.setEvent(event);
        actualSpot.setPlannedEvent(plannedEvent);
        fillLocationInformationByRequiredEvent(actualSpot, event, plannedEvent, locations);
        return actualSpot;
    }

    private void fillLocationInformationByRequiredEvent(
            final ActualSpot actualSpot,
            final Event event,
            final PlannedEvent plannedEvent,
            final List<Location> locations) {
        if (shouldBeFilledWithOwnActualLocation(event)) {
            final String locationAltKey = event.getLocationAltKey();
            fillWithOwnActualLocationInformation(locations, locationAltKey, actualSpot, event);
        } else if (shouldBeFilledWithMasterPlannedLocation(plannedEvent)) {
            final String locationAltKey = plannedEvent.getLocationAltKey();
            fillWithMasterLocationInformation(locations, locationAltKey, actualSpot);
        }
        if (shouldBeFilledWithMasterActualLocation(actualSpot)) {
            final String locationAltKey = event.getLocationAltKey();
            fillWithMasterLocationInformation(locations, locationAltKey, actualSpot);
        }
    }

    private void fillWithOwnActualLocationInformation(
            final List<Location> locations,
            final String locationAltKey,
            final ActualSpot actualSpot,
            final Event event) {
        actualSpot.setLatitude(event.getLatitude());
        actualSpot.setLongitude(event.getLongitude());
        actualSpot.setLocationMasterData(false);
        final Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
        if (locationOpt.isPresent()) {
            final Location location = locationOpt.get();
            fillLocationInformation(location, actualSpot);
        }
    }

    private void fillWithMasterLocationInformation(
            final List<Location> locations, final String locationAltKey, final ActualSpot actualSpot) {
        final Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
        if (locationOpt.isPresent()) {
            final Location location = locationOpt.get();
            actualSpot.setLocationMasterData(true);
            actualSpot.setLatitude(location.getLatitude());
            actualSpot.setLongitude(location.getLongitude());
            fillLocationInformation(location, actualSpot);
        }
    }

    private void fillLocationInformation(final Location location, final ActualSpot actualSpot) {
        actualSpot.setObjectTypeCode(location.getObjectTypeCode());
        actualSpot.setLocationTypeCode(location.getLocationTypeCode());
        actualSpot.setLocationDescription(location.getLocationDescription());
        actualSpot.setLocationId(location.getLocationId());
    }

    private boolean shouldBeFilledWithOwnActualLocation(final Event event) {
        final BigDecimal latitude = event.getLatitude();
        final BigDecimal longitude = event.getLongitude();
        return coordinatesValidator.isValid(longitude, latitude);
    }

    private boolean shouldBeFilledWithMasterPlannedLocation(final PlannedEvent plannedEvent) {
        return nonNull(plannedEvent);
    }

    private boolean shouldBeFilledWithMasterActualLocation(final ActualSpot actualSpot) {
        final BigDecimal longitude = actualSpot.getLongitude();
        final BigDecimal latitude = actualSpot.getLatitude();
        return !coordinatesValidator.isValid(longitude, latitude);
    }
}
