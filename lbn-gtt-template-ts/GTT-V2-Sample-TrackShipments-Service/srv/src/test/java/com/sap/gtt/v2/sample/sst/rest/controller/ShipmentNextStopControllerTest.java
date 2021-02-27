package com.sap.gtt.v2.sample.sst.rest.controller;

import static java.util.UUID.randomUUID;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.NextStopService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ShipmentNextStopControllerTest {

    @Mock
    private NextStopService nextStopService;
    @InjectMocks
    private ShipmentNextStopController shipmentNextStopController;

    @Test
    void getByShipmentId_givenShipmentId_shouldCallAllServices() {
        // given
        final String shipmentId = randomUUID().toString();

        // when
        shipmentNextStopController.getByShipmentId(shipmentId);

        // then
        verify(nextStopService, times(1)).getByTrackedProcessId(shipmentId);
    }
}
