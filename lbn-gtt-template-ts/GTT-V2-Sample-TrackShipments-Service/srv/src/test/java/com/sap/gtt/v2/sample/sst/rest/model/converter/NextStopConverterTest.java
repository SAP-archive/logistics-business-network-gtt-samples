package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class NextStopConverterTest {

    @Mock
    private PlannedEventService plannedEventService;
    @Mock
    private LocationService locationService;
    @InjectMocks
    private NextStopConverter nextStopConverter;

    @Test
    void fromEstimatedArrival_givenEstimatedArrivalAndShipmentId_shouldConvertToNextStop() {
        // given
        final EstimatedArrival estimatedArrival = new EstimatedArrival();
        estimatedArrival.setStopId("1");
        final String shipmentId = randomUUID().toString();
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();

        when(plannedEventService.getAllByTrackedProcessId(shipmentId)).thenReturn(plannedEvents);
        when(locationService.getByAltKey(any())).thenReturn(Optional.of(new Location()));

        // when
        final NextStop nextStop = nextStopConverter.fromEstimatedArrival(estimatedArrival, shipmentId);

        // then
        assertThat(nextStop).isNotNull();
    }
}
