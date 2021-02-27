package com.sap.gtt.v2.sample.sst.rest.controller;

import static java.util.UUID.randomUUID;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.ShipmentEventsByStatusService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ShipmentEventsByStatusControllerTest {

    @Mock
    private ShipmentEventsByStatusService shipmentEventsByStatusService;
    @InjectMocks
    private ShipmentEventsByStatusController shipmentEventsByStatusController;

    @Test
    void getByShipmentId_givenShipmentId_shouldCallAllServices() {
        // given
        final String shipmentId = randomUUID().toString();

        // when
        shipmentEventsByStatusController.getByShipmentId(shipmentId);

        // then
        verify(shipmentEventsByStatusService, times(1)).getByTrackedProcessId(shipmentId);
    }
}
