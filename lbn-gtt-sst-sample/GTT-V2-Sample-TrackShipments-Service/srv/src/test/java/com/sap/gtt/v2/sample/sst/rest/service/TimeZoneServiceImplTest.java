package com.sap.gtt.v2.sample.sst.rest.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.rest.model.TimeZoneList;
import org.junit.jupiter.api.Test;

class TimeZoneServiceImplTest {

    private final TimeZoneService timeZoneService = new TimeZoneServiceImpl();

    @Test
    void getAllTimeZones_shouldReturnAllTimeZones() {
        // when
        final TimeZoneList timeZones = timeZoneService.getAllTimeZones();

        // then
        assertThat(timeZones.getItems()).isNotEmpty();
    }
}
