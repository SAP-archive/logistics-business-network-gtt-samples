package com.sap.gtt.v2.sample.sst.rest.service;

import static java.math.BigDecimal.ONE;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.helper.FreightUnitHelper;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.FreightUnitService;
import com.sap.gtt.v2.sample.sst.odata.service.FreightUnitStopsForVpService;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.helper.ActualSpotHelper;
import com.sap.gtt.v2.sample.sst.rest.helper.RouteHelper;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import com.sap.gtt.v2.sample.sst.rest.model.converter.PlannedSpotConverter;
import com.sap.gtt.v2.sample.sst.rest.validator.RouteValidator;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FreightUnitRouteServiceTest {

    @Mock
    private CoordinatesValidator coordinatesValidator;
    @Mock
    private RouteValidator routeValidator;
    @Mock
    private PlannedEventService plannedEventService;
    @Mock
    private ProcessEventDirectoryService processEventDirectoryService;
    @Mock
    private FreightUnitStopsForVpService freightUnitStopsForVpService;
    @Mock
    private PlannedSpotConverter plannedSpotConverter;
    @Mock
    private ActualSpotHelper actualSpotHelper;
    @Mock
    private CurrentLocationService currentLocationService;
    @Mock
    private RouteHelper routeHelper;
    @Mock
    private FreightUnitService freightUnitService;
    @Mock
    private FreightUnitHelper freightUnitHelper;
    @Mock
    private EventService eventService;
    @InjectMocks
    private FreightUnitRouteService freightUnitRouteService;

    @Test
    void getByTrackedProcessId_givenFreightUnitId_shouldReturnRoute() {
        // given
        final String freightUnitId = randomUUID().toString();
        final String freightUnitJson = SSTUtils.getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);
        final String shipmentsJson = SSTUtils.getStringFromResource("/odata/shipments.json");
        final List<Shipment> shipments = ODataUtils.readEntitySet(shipmentsJson, Shipment.class).getResults();
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();
        final StopsForVp stopForVp = new StopsForVp();
        final Location location = new Location();
        location.setLongitude(ONE);
        location.setLatitude(ONE);
        stopForVp.setLocation(location);
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLatitude(ONE);
        actualSpot.setLongitude(ONE);
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventStatusCode("PLANNED");
        final CurrentLocation currentLocation = new CurrentLocation();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setId(UUID.randomUUID());
        currentLocation.setPlannedEvent(plannedEvent);
        currentLocation.setEstimatedArrival(emptyList());

        when(freightUnitService.getByUri(anyString())).thenReturn(Optional.of(freightUnit));
        when(freightUnitHelper.retrieveShipments(freightUnit)).thenReturn(shipments);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);
        when(plannedEventService.getAllByTrackedProcessId(freightUnitId)).thenReturn(plannedEvents);
        when(freightUnitStopsForVpService.getAll(freightUnitId)).thenReturn(singletonList(stopForVp));
        when(processEventDirectoryService.getByTrackedProcessId(freightUnitId)).thenReturn(processEventDirectories);
        when(actualSpotHelper.getAllAscending(any())).thenReturn(singletonList(actualSpot));
        when(currentLocationService.getFromActualSpots(any())).thenReturn(Optional.of(currentLocation));
        when(plannedSpotConverter.fromPlannedEvents(any())).thenReturn(singletonList(plannedSpot));

        // when
        final Route route = freightUnitRouteService.getByTrackedProcessId(freightUnitId);

        // then
        assertThat(route).isNotNull();
        assertThat(route).extracting(Route::getStopsForVp).isNotNull();
        assertThat(route).extracting(Route::getActualSpots).isNotNull();
        assertThat(route).extracting(Route::getPlannedSpots).isNotNull();
        assertThat(route).extracting(Route::getCurrentLocation).isNotNull();
    }

    @Test
    void getByTrackedProcessId_givenFreightUnitIdWithNextPlannedSpot_shouldReturnRoute() {
        // given
        final String freightUnitId = randomUUID().toString();
        final String freightUnitJson = SSTUtils.getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);
        final String shipmentsJson = SSTUtils.getStringFromResource("/odata/shipments.json");
        final List<Shipment> shipments = ODataUtils.readEntitySet(shipmentsJson, Shipment.class).getResults();
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();
        final StopsForVp stopForVp = new StopsForVp();
        final Location location = new Location();
        location.setLongitude(ONE);
        location.setLatitude(ONE);
        stopForVp.setLocation(location);
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLatitude(ONE);
        actualSpot.setLongitude(ONE);
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventId(UUID.randomUUID());
        plannedSpot.setEventStatusCode("PLANNED");
        final CurrentLocation currentLocation = new CurrentLocation();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setId(UUID.randomUUID());
        currentLocation.setPlannedEvent(plannedEvent);
        currentLocation.setEstimatedArrival(emptyList());
        final PlannedSpot nextPlannedSpot = new PlannedSpot();
        nextPlannedSpot.setEventId(UUID.randomUUID());

        when(freightUnitService.getByUri(anyString())).thenReturn(Optional.of(freightUnit));
        when(freightUnitHelper.retrieveShipments(freightUnit)).thenReturn(shipments);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);
        when(plannedEventService.getAllByTrackedProcessId(freightUnitId)).thenReturn(plannedEvents);
        when(freightUnitStopsForVpService.getAll(freightUnitId)).thenReturn(singletonList(stopForVp));
        when(processEventDirectoryService.getByTrackedProcessId(freightUnitId)).thenReturn(processEventDirectories);
        when(actualSpotHelper.getAllAscending(any())).thenReturn(singletonList(actualSpot));
        when(currentLocationService.getFromActualSpots(any())).thenReturn(Optional.of(currentLocation));
        when(plannedSpotConverter.fromPlannedEvents(any())).thenReturn(singletonList(plannedSpot));

        // when
        final Route route = freightUnitRouteService.getByTrackedProcessId(freightUnitId);

        // then
        assertThat(route).isNotNull();
        assertThat(route).extracting(Route::getStopsForVp).isNotNull();
        assertThat(route).extracting(Route::getActualSpots).isNotNull();
        assertThat(route).extracting(Route::getPlannedSpots).isNotNull();
        assertThat(route).extracting(Route::getCurrentLocation).isNotNull();
    }
}
