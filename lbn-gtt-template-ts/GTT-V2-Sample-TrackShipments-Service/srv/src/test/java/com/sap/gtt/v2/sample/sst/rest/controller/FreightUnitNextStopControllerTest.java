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
class FreightUnitNextStopControllerTest {

    @Mock
    private NextStopService nextStopService;
    @InjectMocks
    private FreightUnitNextStopController freightUnitNextStopController;

    @Test
    void getByFreightUnitId_givenFreightUnitId_shouldCallAllServices() {
        // given
        final String freightUnitId = randomUUID().toString();

        // when
        freightUnitNextStopController.getByFreightUnitId(freightUnitId);

        // then
        verify(nextStopService, times(1)).getByTrackedProcessId(freightUnitId);
    }
}