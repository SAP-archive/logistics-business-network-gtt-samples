package com.sap.gtt.v2.sample.sst.rest.controller;

import static java.util.UUID.randomUUID;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.ShipmentTimelineEventService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ShipmentTimelineEventControllerTest {

    @Mock
    private ShipmentTimelineEventService shipmentTimelineEventService;
    @InjectMocks
    private ShipmentTimelineEventController shipmentTimelineEventController;

    @Test
    void getByShipmentId_givenShipmentId_shouldCallAllServices() {
        // given
        final String shipmentId = randomUUID().toString();

        // when
        shipmentTimelineEventController.getByShipmentId(shipmentId);

        // then
        verify(shipmentTimelineEventService, times(1)).getByTrackedProcessId(shipmentId);
    }
}
