package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TimelineEventConverterTest {

    @Mock
    private LocationService locationService;
    @Mock
    private EventService eventService;
    @InjectMocks
    private TimelineEventConverter timelineEventConverter;

    @Test
    void fromPlannedEvents_givenPlannedEvents_shouldReturnTimelineEvents() {
        // given
        final String plannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-events.json");
        final List<PlannedEvent> plannedEvents = ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class).getResults();
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(locationService.getAll()).thenReturn(locations);

        // when
        final List<TimelineEvent> timelineEvents = timelineEventConverter.fromPlannedEvents(plannedEvents);

        // then
        assertThat(timelineEvents).isNotEmpty();
    }

    @Test
    void fromActualEvents_givenActualEventsWithoutPlannedEvents_shouldReturnTimelineEvents() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final ProcessEventDirectory processEventDirectory =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults().get(0);
        processEventDirectory.setPlannedEvent(null);
        final Map<ProcessEventDirectory, List<EventHistory>> actualEvents = new HashMap<>();
        actualEvents.put(processEventDirectory, emptyList());

        // when
        final List<TimelineEvent> timelineEvents = timelineEventConverter.fromActualEvents(actualEvents);

        // then
        assertThat(timelineEvents).isNotEmpty();
    }

    @Test
    void fromActualEvents_givenActualEventsWithPlannedEvents_shouldReturnTimelineEvents() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final ProcessEventDirectory processEventDirectory =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults().get(0);
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setEventType("test.Test");
        processEventDirectory.setPlannedEvent(plannedEvent);
        final Map<ProcessEventDirectory, List<EventHistory>> actualEvents = new HashMap<>();
        actualEvents.put(processEventDirectory, emptyList());

        // when
        final List<TimelineEvent> timelineEvents = timelineEventConverter.fromActualEvents(actualEvents);

        // then
        assertThat(timelineEvents).isNotEmpty();
    }
}
