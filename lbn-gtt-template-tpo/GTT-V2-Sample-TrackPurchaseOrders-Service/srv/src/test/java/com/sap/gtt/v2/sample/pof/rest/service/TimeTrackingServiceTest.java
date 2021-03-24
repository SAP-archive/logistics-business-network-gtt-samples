package com.sap.gtt.v2.sample.pof.rest.service;


import static com.sap.gtt.v2.sample.pof.utils.TimelineEventConverter.UNPLANNED;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.EventHistory;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.LocationService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl.PlannedEventServiceImpl;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl.ProcessEventDirectoryServiceImpl;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl.TimeTrackingServiceImpl;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import com.sap.gtt.v2.sample.pof.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.pof.utils.TimelineEventConverter;
import java.io.IOException;
import java.util.List;
import org.apache.commons.io.IOUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.io.ClassPathResource;


@RunWith(PowerMockRunner.class)
@PrepareForTest(ProcessEventDirectoryUtils.class)
public class TimeTrackingServiceTest {
    @Mock
    private PlannedEventServiceImpl plannedEventService;
    @Mock
    private ProcessEventDirectoryServiceImpl processEventDirectoryService;
    @Mock
    private LocationService locationService;
    @Mock
    private TimelineEventConverter timelineEventConverter;
    @InjectMocks
    private TimeTrackingServiceImpl service;

    @Test
    public void testGetByDeliveryItemId() throws IOException {
        String deliveryItemId = "c75316ce-a2cd-5f8c-82e4-b4661d3a48e2";

        String pedJson = IOUtils.toString(new ClassPathResource("/rest/timeline-ped.json").getInputStream(), "UTF-8");
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(pedJson, ProcessEventDirectory.class);
        List<ProcessEventDirectory> processEventDirectories = processEventDirectoryODataResultList.getResults();
        Mockito.when(processEventDirectoryService.getAllByDeliveryItemId(deliveryItemId)).thenReturn(
                processEventDirectories);

        String plannedEventJson = IOUtils.toString(new ClassPathResource("/rest/timeline-plannedEvent.json").getInputStream(), "UTF-8");
        ODataResultList<PlannedEvent> plannedEventODataResultList = ODataUtils.readEntitySet(plannedEventJson, PlannedEvent.class);
        Mockito.when(plannedEventService.getAllByDeliveryItemId(deliveryItemId)).thenReturn(
                plannedEventODataResultList.getResults());

        Mockito.doNothing().when(locationService).fillLocations(Mockito.anyList());
        Mockito.doCallRealMethod().when(timelineEventConverter).toPlannedEvents(Mockito.anyList());
        Mockito.doCallRealMethod().when(timelineEventConverter).toUnplannedEvents(Mockito.anyList());
        Mockito.doCallRealMethod().when(timelineEventConverter).toReportedPlannedEvents(Mockito.anyList(),Mockito.anyMap());
        Mockito.doCallRealMethod().when(processEventDirectoryService).getWithoutPlannedEvent(Mockito.anyList());

        List<TimelineEvent> res = service.getByDeliveryItemId(deliveryItemId);

        assertEquals(4,res.size());
        Long plannedEvent_pt1 = POFUtils.getDateTimeLong(res.get(2).getPlannedBusinessTimestamp());
        String plannedEvent_at1 = res.get(2).getActualBusinessTimestamp();
        Long plannedEvent_pt2 = POFUtils.getDateTimeLong(res.get(3).getPlannedBusinessTimestamp());
        String plannedEvent_at2 = res.get(3).getActualBusinessTimestamp();
        Long actualEvent_at1 = POFUtils.getDateTimeLong(res.get(0).getActualBusinessTimestamp());
        Long actualEvent_at2 = POFUtils.getDateTimeLong(res.get(1).getActualBusinessTimestamp());

        assertTrue(plannedEvent_pt1>=plannedEvent_pt2);
        assertNull(plannedEvent_at1);
        assertNull(plannedEvent_at2);
        assertTrue(actualEvent_at1>=actualEvent_at2);

        assertEquals(UNPLANNED,res.get(0).getEventStatusCode());

        assertEquals(2,res.get(1).getEventHistory().size());
        List<EventHistory> eventHistories = res.get(1).getEventHistory();
        Long eventHistory_att1 = eventHistories.get(0).getActualTechTimestamp();
        Long eventHistory_att2 = eventHistories.get(1).getActualTechTimestamp();
        assertTrue(eventHistory_att1>=eventHistory_att2);

        Long eventHistory_at1 = POFUtils.getDateTimeLong(eventHistories.get(0).getActualBusinessTimestamp());
        Long eventHistory_at2 = POFUtils.getDateTimeLong(eventHistories.get(1).getActualBusinessTimestamp());
        Long reportedPlannedEvent_at = POFUtils.getDateTimeLong(res.get(1).getActualBusinessTimestamp());

        assertEquals(eventHistory_at2,reportedPlannedEvent_at);

    }
}

