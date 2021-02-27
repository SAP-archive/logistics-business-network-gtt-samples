package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.helper.PlannedSpotHelper;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.PlannedSpotConverter;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PlannedSpotHelperTest {

    @Mock
    private PlannedSpotConverter plannedSpotConverter;
    @Mock
    private CoordinatesValidator coordinatesValidator;
    @InjectMocks
    private PlannedSpotHelper plannedSpotHelper;

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithWrongEventType_shouldReturnEmpty() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Test");

        // when
        final Optional<PlannedSpot> nextPlannedSpotOpt = plannedSpotHelper.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpotOpt).isNotPresent();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithNullRefPlannedEventType_shouldReturnEmpty() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Delay");

        // when
        final Optional<PlannedSpot> nextPlannedSpotOpt = plannedSpotHelper.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpotOpt).isNotPresent();
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
        final Optional<PlannedSpot> nextPlannedSpotOpt = plannedSpotHelper.findNextPlannedSpot(currentLocation, singletonList(plannedSpot), emptyList());

        // then
        assertThat(nextPlannedSpotOpt).isPresent();
    }

    @Test
    void findNextPlannedSpot_givenCurrentLocationWithPlannedEvent_shouldReturnEmpty() {
        // given
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEventType("test.Test");
        currentLocation.setPlannedEvent(new PlannedEvent());

        // when
        final Optional<PlannedSpot> nextPlannedSpotOpt = plannedSpotHelper.findNextPlannedSpot(currentLocation, emptyList(), emptyList());

        // then
        assertThat(nextPlannedSpotOpt).isNotPresent();
    }
}
