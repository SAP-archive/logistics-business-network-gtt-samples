package com.sap.gtt.v2.sample.sst.rest.helper;

import static java.math.BigDecimal.ONE;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RouteHelperTest {

    @Mock
    private PlannedSpotHelper plannedSpotHelper;
    @InjectMocks
    private RouteHelper routeHelper;

    @Test
    void updateRoutesConnection_givenRoute_shouldUpdateConnections() {
        //given
        final Route route = new Route();
        final CurrentLocation currentLocation = new CurrentLocation();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setId(randomUUID());
        currentLocation.setPlannedEvent(plannedEvent);
        currentLocation.setEstimatedArrival(emptyList());
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventStatusCode("PLANNED");
        plannedSpot.setEventId(randomUUID());
        route.setCurrentLocation(currentLocation);
        route.setPlannedSpots(new ArrayList<>(singletonList(plannedSpot)));
        final PlannedSpot nextPlannedSpot = new PlannedSpot();
        nextPlannedSpot.setEventId(randomUUID());
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();

        when(plannedSpotHelper.findNextPlannedSpot(eq(currentLocation), eq(route.getPlannedSpots()), eq(plannedEvents)))
                .thenReturn(Optional.of(nextPlannedSpot));

        // when
        routeHelper.updateRoutesConnection(route, plannedEvents);

        // then
        assertThat(route.isToConnectPlannedAndActualRoute()).isTrue();
    }

    @Test
    void updateEventStatusOfStopsForVp_givenRoute_shouldUpdate_StopsForVp() {
        // given
        final Route route = new Route();
        final StopsForVp stopForVp = new StopsForVp();
        final Location location = new Location();
        location.setLongitude(ONE);
        location.setLatitude(ONE);
        stopForVp.setLocation(location);
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventStatusCode("PLANNED");
        plannedSpot.setEventId(randomUUID());
        route.setStopsForVp(new ArrayList<>(singletonList(stopForVp)));
        route.setPlannedSpots(new ArrayList<>(singletonList(plannedSpot)));
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();

        // when-then
        assertDoesNotThrow(() -> routeHelper.updateEventStatusOfStopsForVp(route, plannedEvents));
    }
}
