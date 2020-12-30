package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import com.sap.gtt.v2.sample.sst.rest.model.converter.NextStopConverter;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class NextStopServiceImplTest {

    @Mock
    private CurrentLocationService currentLocationService;
    @Mock
    private NextStopConverter nextStopConverter;
    @InjectMocks
    private NextStopServiceImpl nextStopService;

    @Test
    void getByShipmentId_givenShipmentId_shouldReturnNextStop() {
        // given
        final String shipmentId = randomUUID().toString();
        final CurrentLocation currentLocation = new CurrentLocation();
        final EstimatedArrival estimatedArrival = new EstimatedArrival();
        estimatedArrival.setStopId("1");
        currentLocation.setEstimatedArrival(singletonList(estimatedArrival));

        when(currentLocationService.getByShipmentId(shipmentId)).thenReturn(Optional.of(currentLocation));
        when(nextStopConverter.fromEstimatedArrival(any(), any())).thenReturn(new NextStop());

        // when
        final Optional<NextStop> nextStopOpt = nextStopService.getByShipmentId(shipmentId);

        // then
        assertThat(nextStopOpt).isPresent();
    }

    @Test
    void getByShipmentId_givenShipmentId_shouldReturnEmptyNextStop() {
        // given
        final String shipmentId = randomUUID().toString();
        final CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setEstimatedArrival(emptyList());

        when(currentLocationService.getByShipmentId(shipmentId)).thenReturn(Optional.of(currentLocation));

        // when
        final Optional<NextStop> nextStopOpt = nextStopService.getByShipmentId(shipmentId);

        // then
        assertThat(nextStopOpt).isNotPresent();
    }
}
