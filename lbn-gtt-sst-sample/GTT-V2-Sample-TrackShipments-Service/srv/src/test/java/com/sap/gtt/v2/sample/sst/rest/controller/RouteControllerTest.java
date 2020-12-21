package com.sap.gtt.v2.sample.sst.rest.controller;

import static java.util.UUID.randomUUID;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.RouteService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class RouteControllerTest {

    @Mock
    private RouteService routeService;
    @InjectMocks
    private RouteController routeController;

    @Test
    void getByShipmentId_givenShipmentId_shouldCallAllServices() {
        // given
        final String shipmentId = randomUUID().toString();

        // when
        routeController.getByShipmentId(shipmentId);

        // then
        verify(routeService, times(1)).getByShipmentId(shipmentId);
    }
}
