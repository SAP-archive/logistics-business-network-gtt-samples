package com.sap.gtt.v2.sample.sst.rest.validator;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RouteValidatorTest {

    @Mock
    private CoordinatesValidator coordinatesValidator;
    @InjectMocks
    private RouteValidator routeValidator;

    @Test
    void validateStopsForVp_givenRouteWithStopsForVp_shouldPassValidation() {
        // given
        final Route route = new Route();
        final StopsForVp stopForVp = new StopsForVp();
        stopForVp.setLocation(new Location());

        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);

        // when
        routeValidator.validateStopsForVp(route, singletonList(stopForVp));

        // then
        assertThat(route.isHasLocationWithInvalidGeo()).isFalse();
    }

    @Test
    void validateStopsForVp_givenRouteWithStopsForVp_shouldFailValidation() {
        // given
        final Route route = new Route();
        final StopsForVp stopForVp = new StopsForVp();
        stopForVp.setLocation(new Location());

        when(coordinatesValidator.isValid(any(), any())).thenReturn(false);

        // when
        routeValidator.validateStopsForVp(route, singletonList(stopForVp));

        // then
        assertThat(route.isHasLocationWithInvalidGeo()).isTrue();
    }

    @Test
    void validateActualSpots_givenRouteAndActualSpotsWithLocationMasterData_shouldPassValidation() {
        // given
        final Route route = new Route();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLocationMasterData(true);

        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);

        // when
        routeValidator.validateActualSpots(route, singletonList(actualSpot));

        // then
        assertThat(route.isHasLocationWithInvalidGeo()).isFalse();
    }

    @Test
    void validateActualSpots_givenRouteAndActualSpotsWithoutLocationMasterData_shouldPassValidation() {
        // given
        final Route route = new Route();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLocationMasterData(false);

        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);

        // when
        routeValidator.validateActualSpots(route, singletonList(actualSpot));

        // then
        assertThat(route.isHasActualEventWithInvalidGeo()).isFalse();
    }

    @Test
    void validatePlannedSpots_givenRouteAndPlannedSpots_shouldPassValidation() {
        // given
        final Route route = new Route();
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setLocationAltKey("test");

        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);

        // when
        routeValidator.validatePlannedSpots(route, singletonList(plannedSpot));

        // then
        assertThat(route.isHasLocationWithInvalidGeo()).isFalse();
    }

    @Test
    void validatePlannedSpots_givenRouteAndPlannedSpots_shouldFailValidation() {
        // given
        final Route route = new Route();
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setLocationAltKey("test");

        when(coordinatesValidator.isValid(any(), any())).thenReturn(false);

        // when
        routeValidator.validatePlannedSpots(route, singletonList(plannedSpot));

        // then
        assertThat(route.isHasLocationWithInvalidGeo()).isTrue();
    }
}
