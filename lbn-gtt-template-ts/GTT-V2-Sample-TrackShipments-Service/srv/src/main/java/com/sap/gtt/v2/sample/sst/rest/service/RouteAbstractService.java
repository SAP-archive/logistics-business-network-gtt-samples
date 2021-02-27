package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.isPlannedEventNotBeenReported;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.helper.ActualSpotHelper;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import com.sap.gtt.v2.sample.sst.rest.model.converter.PlannedSpotConverter;
import com.sap.gtt.v2.sample.sst.rest.validator.RouteValidator;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * {@link RouteAbstractService} is an abstract service which operates on {@link Route} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Service
public abstract class RouteAbstractService {

    @Autowired
    protected CoordinatesValidator coordinatesValidator;

    @Autowired
    protected RouteValidator routeValidator;

    @Autowired
    protected PlannedSpotConverter plannedSpotConverter;

    @Autowired
    protected ActualSpotHelper actualSpotHelper;

    @Autowired
    protected CurrentLocationService currentLocationService;

    /**
     * Retrieves {@link Route} by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return {@link Route} entity
     */
    public abstract Route getByTrackedProcessId(@NotNull final String trackedProcessId);

    /**
     * Retrieves {@link StopsForVp} of provided tracked process, validates them and fills
     * {@link Route} with correct {@link StopsForVp}.
     *
     * @param route      - {@link Route} entity of tracked process
     * @param stopsForVp - list of {@link StopsForVp} of tracked process
     */
    protected void validateAndFillStopsForVp(@NotNull final Route route, @NotNull final List<StopsForVp> stopsForVp) {
        final List<StopsForVp> filteredStopsForVp = filterOutStopsForVpWithInvalidCoordinates(stopsForVp);
        routeValidator.validateStopsForVp(route, stopsForVp);
        route.setStopsForVp(filteredStopsForVp);
    }

    /**
     * Retrieves {@link ActualSpot} of provided tracked process, validates them and fills
     * {@link Route} with correct {@link ActualSpot}.
     *
     * @param route       - {@link Route} entity of tracked process
     * @param actualSpots - list of {@link ActualSpot} of tracked process
     */
    protected void validateAndFillActualSpots(@NotNull final Route route, @NotNull final List<ActualSpot> actualSpots) {
        final List<ActualSpot> filteredActualSpots = filterOutActualSpotsWithInvalidCoordinates(actualSpots);
        routeValidator.validateActualSpots(route, actualSpots);
        route.setActualSpots(filteredActualSpots);
    }

    /**
     * Retrieves {@link CurrentLocation} of tracked process and fills {@link Route} with it.
     *
     * @param route - {@link Route} entity of tracked process
     */
    protected void fillCurrentLocation(@NotNull final Route route) {
        final List<ActualSpot> actualSpots = route.getActualSpots();
        final Optional<CurrentLocation> currentLocation = currentLocationService.getFromActualSpots(actualSpots);
        currentLocation.ifPresent(route::setCurrentLocation);
    }

    /**
     * Retrieves {@link PlannedSpot} of provided tracked process, validates them and fills
     * {@link Route} with correct {@link PlannedSpot}.
     *
     * @param route        - {@link Route} entity of tracked process
     * @param plannedSpots - list of {@link PlannedSpot} of tracked process
     */
    protected void validateAndFillPlannedSpots(@NotNull final Route route, @NotNull final List<PlannedSpot> plannedSpots) {
        final List<PlannedSpot> filteredPlannedSpots = filterOutPlannedSpotsWithConditions(plannedSpots);
        routeValidator.validatePlannedSpots(route, plannedSpots);
        route.setPlannedSpots(filteredPlannedSpots);
    }

    private List<ActualSpot> filterOutActualSpotsWithInvalidCoordinates(final List<ActualSpot> actualSpots) {
        return actualSpots.stream()
                .filter(actualSpot -> coordinatesValidator.isValid(actualSpot.getLongitude(), actualSpot.getLatitude()))
                .collect(toList());
    }

    private List<StopsForVp> filterOutStopsForVpWithInvalidCoordinates(final List<StopsForVp> stopsForVp) {
        return stopsForVp.stream()
                .filter(stopForVp -> nonNull(stopForVp.getLocation()))
                .filter(stopForVp -> coordinatesValidator.isValid(
                        stopForVp.getLocation().getLongitude(),
                        stopForVp.getLocation().getLatitude()))
                .collect(toList());
    }

    private List<PlannedSpot> filterOutPlannedSpotsWithConditions(final List<PlannedSpot> plannedSpots) {
        return plannedSpots.stream()
                .filter(plannedSpot -> coordinatesValidator.isValid(plannedSpot.getLongitude(), plannedSpot.getLatitude()))
                .filter(plannedSpot -> isPlannedEventNotBeenReported(plannedSpot.getEventStatusCode()))
                .collect(toList());
    }
}
