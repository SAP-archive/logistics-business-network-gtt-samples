package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PlannedSpotConverterTest {

    @Mock
    private LocationService locationService;
    @InjectMocks
    private PlannedSpotConverter plannedSpotConverter;

    @Test
    void fromPlannedEvents_givenPlannedEvents_shouldConvertToPlannedSpots() {
        // given
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(locationService.getAll()).thenReturn(locations);

        // when
        final List<PlannedSpot> plannedSpots = plannedSpotConverter.fromPlannedEvents(plannedEvents);

        // then
        assertThat(plannedSpots).isNotEmpty();
    }
}
