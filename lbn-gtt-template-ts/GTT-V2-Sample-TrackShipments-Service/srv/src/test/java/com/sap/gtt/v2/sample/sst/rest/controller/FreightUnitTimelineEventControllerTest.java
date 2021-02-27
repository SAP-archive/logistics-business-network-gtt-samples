package com.sap.gtt.v2.sample.sst.rest.controller;

import static java.util.UUID.randomUUID;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.FreightUnitTimelineEventService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FreightUnitTimelineEventControllerTest {

    @Mock
    private FreightUnitTimelineEventService freightUnitTimelineEventService;
    @InjectMocks
    private FreightUnitTimelineEventController freightUnitTimelineEventController;

    @Test
    void getByFreightUnitId_givenFreightUnitId_shouldCallAllServices() {
        // given
        final String freightUnitId = randomUUID().toString();

        // when
        freightUnitTimelineEventController.getByFreightUnitId(freightUnitId);

        // then
        verify(freightUnitTimelineEventService, times(1)).getByTrackedProcessId(freightUnitId);
    }
}