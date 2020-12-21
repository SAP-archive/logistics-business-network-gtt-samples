package com.sap.gtt.v2.sample.sst.rest.controller;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.TimeZoneService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TimeZoneControllerTest {

    @Mock
    private TimeZoneService timeZoneService;
    @InjectMocks
    private TimeZoneController timeZoneController;

    @Test
    void getAllTimeZones_shouldCallAllServices() {
        // when
        timeZoneController.getAllTimeZones();

        // then
        verify(timeZoneService, times(1)).getAllTimeZones();
    }
}
