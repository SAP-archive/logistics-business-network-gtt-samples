package com.sap.gtt.v2.sample.sst.rest.validator;

import static java.util.Objects.nonNull;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link RouteValidator} is a validator which verifies that provided coordinates are valid.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class RouteValidator {

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    /**
     * Validates coordinates of {@link StopsForVp} entities.
     *
     * @param route      - {@link Route} entity
     * @param stopsForVp - {@link StopsForVp} entities
     */
    public void validateStopsForVp(@NotNull final Route route, @NotNull final List<StopsForVp> stopsForVp) {
        route.setHasLocationWithInvalidGeo(stopsForVpHaveInvalidCoordinates(stopsForVp));
    }

    /**
     * Validates coordinates of {@link ActualSpot} entities.
     *
     * @param route       - {@link Route} entity
     * @param actualSpots - {@link ActualSpot} entities
     */
    public void validateActualSpots(@NotNull final Route route, @NotNull final List<ActualSpot> actualSpots) {
        actualSpots.forEach(actualSpot -> validateActualSpotsCoordinates(actualSpot, route));
    }

    /**
     * Validates coordinates of {@link PlannedSpot} entities.
     *
     * @param route        - {@link Route} entity
     * @param plannedSpots - {@link PlannedSpot} entities
     */
    public void validatePlannedSpots(@NotNull final Route route, @NotNull final List<PlannedSpot> plannedSpots) {
        route.setHasLocationWithInvalidGeo(plannedSpotsHaveInvalidCoordinates(plannedSpots));
    }

    private void validateActualSpotsCoordinates(final ActualSpot actualSpot, final Route route) {
        final BigDecimal longitude = actualSpot.getLongitude();
        final BigDecimal latitude = actualSpot.getLatitude();
        final boolean isInvalidCoordinates = !coordinatesValidator.isValid(longitude, latitude);
        if (actualSpot.isLocationMasterData()) {
            route.setHasLocationWithInvalidGeo(isInvalidCoordinates);
        } else {
            route.setHasActualEventWithInvalidGeo(isInvalidCoordinates);
        }
    }

    private boolean stopsForVpHaveInvalidCoordinates(final List<StopsForVp> stopsForVp) {
        return stopsForVp.stream()
                .map(StopsForVp::getLocation)
                .filter(Objects::nonNull)
                .anyMatch(location -> !coordinatesValidator.isValid(location.getLongitude(), location.getLatitude()));
    }

    private boolean plannedSpotsHaveInvalidCoordinates(final List<PlannedSpot> plannedSpots) {
        return plannedSpots.stream()
                .filter(plannedSpot -> nonNull(plannedSpot.getLocationAltKey()))
                .anyMatch(plannedSpot -> !coordinatesValidator.isValid(plannedSpot.getLongitude(), plannedSpot.getLatitude()));
    }
}
