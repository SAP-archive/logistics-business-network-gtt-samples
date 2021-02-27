package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.helper.TimelineEventHelper;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import com.sap.gtt.v2.sample.sst.rest.model.converter.TimelineEventConverter;
import java.util.HashMap;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class FreightUnitTimelineEventServiceTest {

    @Mock
    private PlannedEventService plannedEventService;
    @Mock
    private EventHistoryService eventHistoryService;
    @Mock
    private ProcessEventDirectoryService processEventDirectoryService;
    @Mock
    private TimelineEventConverter timelineEventConverter;
    @Mock
    private TimelineEventHelper timelineEventHelper;
    @InjectMocks
    private FreightUnitTimelineEventService freightUnitTimelineEventService;

    @Test
    void getByTrackedProcessId_givenFreightUnitId_shouldReturnTimelineEvents() {
        // given
        final String freightUnitId = randomUUID().toString();
        final PlannedEvent plannedEvent = new PlannedEvent();
        plannedEvent.setEventType("PLANNED");
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        when(plannedEventService.getAllByTrackedProcessId(freightUnitId)).thenReturn(singletonList(plannedEvent));
        when(timelineEventConverter.fromPlannedEvents(anyList())).thenReturn(singletonList(new TimelineEvent()));
        when(processEventDirectoryService.getByTrackedProcessId(freightUnitId)).thenReturn(processEventDirectories);
        when(processEventDirectoryService.getWithoutPlannedEvent(anyList())).thenReturn(emptyList());
        when(processEventDirectoryService.getActualEventsByPlannedEvents(anyList())).thenReturn(new HashMap<>());
        when(eventHistoryService.getEventHistoryForPlannedEvents(anyMap())).thenReturn(new HashMap<>());
        when(timelineEventConverter.fromActualEvents(anyMap())).thenReturn(singletonList(new TimelineEvent()));

        // when
        final List<TimelineEvent> timelineEvents = freightUnitTimelineEventService.getByTrackedProcessId(freightUnitId);

        // then
        assertThat(timelineEvents).isNotEmpty();
    }
}