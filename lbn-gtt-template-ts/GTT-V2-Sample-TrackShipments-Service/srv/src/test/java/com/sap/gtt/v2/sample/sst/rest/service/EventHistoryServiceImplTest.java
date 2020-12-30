package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import com.sap.gtt.v2.sample.sst.rest.model.converter.EventHistoryConverter;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class EventHistoryServiceImplTest {

    @Mock
    private EventHistoryConverter eventHistoryConverter;
    @InjectMocks
    private EventHistoryServiceImpl eventHistoryService;

    @Test
    void getEventHistoryForPlannedEvents_givenActualEventsByPlannedEvents_shouldReturnEventHistory() {
        // given
        final ProcessEventDirectory processEventDirectoryOne = new ProcessEventDirectory();
        final ProcessEventDirectory processEventDirectoryTwo = new ProcessEventDirectory();
        final Event event = new Event();
        event.setActualBusinessTimestamp(Instant.MAX.getEpochSecond());
        processEventDirectoryTwo.setEvent(event);
        final Map<ProcessEventDirectory, List<ProcessEventDirectory>> actualEventsByPlannedEvents = new HashMap<>();
        actualEventsByPlannedEvents.put(processEventDirectoryOne, singletonList(processEventDirectoryTwo));

        when(eventHistoryConverter.fromProcessEventDirectory(any())).thenReturn(new EventHistory());

        // when
        final Map<ProcessEventDirectory, List<EventHistory>> result =
                eventHistoryService.getEventHistoryForPlannedEvents(actualEventsByPlannedEvents);

        // then
        verify(eventHistoryConverter, times(1)).fromProcessEventDirectory(any());
        assertThat(result).isNotEmpty();
    }
}
