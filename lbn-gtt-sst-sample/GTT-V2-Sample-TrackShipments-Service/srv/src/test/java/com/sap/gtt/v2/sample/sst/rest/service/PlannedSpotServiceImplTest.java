package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.PlannedSpotConverter;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PlannedSpotServiceImplTest {

    @Mock
    private PlannedSpotConverter plannedSpotConverter;
    @Mock
    private CoordinatesValidator coordinatesValidator;
    @InjectMocks
    private PlannedSpotServiceImpl plannedSpotService;

    @Test
    void getAll_givenPlannedEvents_shouldReturnPlannedSpots() {
        // given
        final List<PlannedEvent> plannedEvents = singletonList(new PlannedEvent());

        when(plannedSpotConverter.fromPlannedEvents(plannedEvents)).thenReturn(singletonList(new PlannedSpot()));

        // when
        final List<PlannedSpot> plannedSpots = plannedSpotService.getAll(plannedEvents);

        // then
        assertThat(plannedSpots).isNotEmpty();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithWrongEventType_shouldReturnNull() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Test");

        // when
        final PlannedSpot nextPlannedSpot = plannedSpotService.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpot).isNull();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithNullRefPlannedEventType_shouldReturnNull() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Delay");

        // when
        final PlannedSpot nextPlannedSpot = plannedSpotService.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpot).isNull();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithValidData_shouldReturnNextPlanedSpot() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Delay");
        currentLocation.setRefPlannedEventType("test");
        currentLocation.setRefPlannedEventMatchKey("test");
        final PlannedSpot plannedSpot = new PlannedSpot();
        plannedSpot.setEventType("test");
        plannedSpot.setEventMatchKey("test");

        // when
        final PlannedSpot nextPlannedSpot = plannedSpotService.findNextPlannedSpot(currentLocation, singletonList(plannedSpot), emptyList());

        // then
        assertThat(nextPlannedSpot).isNotNull();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithPlannedEvent_shouldReturnNull() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Test");
        currentLocation.setPlannedEvent(new PlannedEvent());

        // when
        final PlannedSpot nextPlannedSpot = plannedSpotService.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpot).isNull();
    }
}
