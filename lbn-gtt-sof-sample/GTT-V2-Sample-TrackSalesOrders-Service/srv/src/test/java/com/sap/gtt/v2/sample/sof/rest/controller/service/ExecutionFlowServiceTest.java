package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.domain.EventEx;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.ExecutionFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.Lane;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.Node;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.*;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SOFUtils.class)
public class ExecutionFlowServiceTest {

    @Mock
    private GTTCoreServiceClient client;

    @Mock
    private MapService mapService;

    @Mock
    private SOFService sofService;

    @InjectMocks
    private ExecutionFlowService executionFlowService;

    @Test
    public void testGenerateExecutionFlow() {
        String pedJson = SOFUtils.getStringFromResource("/odata/execution-flow-ped.json");
        String plannedEventJson = SOFUtils.getStringFromResource("/odata/execution-flow-plan-event.json");
        String departureEventJson = SOFUtils.getStringFromResource("/odata/execution-flow-departure-event.json");
        String arrivalEventJson = SOFUtils.getStringFromResource("/odata/execution-flow-arrival-event.json");
        String packingEventJson = SOFUtils.getStringFromResource("/odata/execution-flow-packing-event.json");
        String locationJson = SOFUtils.getStringFromResource("/odata/execution-flow-location.json");

        Mockito.when(client.readEntitySetAll(contains("/ProcessEventDirectory?$filter=process_id eq"), eq(ProcessEventDirectory.class))).thenReturn(ODataUtils.readEntitySet(pedJson, ProcessEventDirectory.class));
        Mockito.when(client.readEntitySetAll(contains("/Departure"), eq(EventEx.class))).thenReturn(ODataUtils.readEntitySet(departureEventJson, EventEx.class));
        Mockito.when(client.readEntitySetAll(contains("/Arrival"), eq(EventEx.class))).thenReturn(ODataUtils.readEntitySet(arrivalEventJson, EventEx.class));
        Mockito.when(client.readEntitySetAll(contains("/Packing"), eq(EventEx.class))).thenReturn(ODataUtils.readEntitySet(packingEventJson, EventEx.class));

        Mockito.when(client.getLocations(any())).thenReturn(ODataUtils.readEntitySet(locationJson, Location.class).getResults());
        Mockito.when(sofService.getPlannedEvents4TP(any())).thenReturn(ODataUtils.readEntitySet(plannedEventJson, PlannedEvent.class).getResults());
        Mockito.when(mapService.getActualRoute(anySet(), anySet(), anyString())).thenReturn(new HashMap<>());

        ExecutionFlow flow = executionFlowService.generateExecutionFlow(UUID.fromString("a49efc6f-10b0-55c3-b313-81bfe350abff"));

        List<Node> nodes = flow.getNodes();
        Node node1 = nodes.get(0);
        Node node2 = nodes.get(2);
        Node node3 = nodes.get(nodes.size() - 1);

        Assert.assertEquals(14, nodes.size());

        Assert.assertEquals("2020-09-10T01:40:06Z", node1.getActualAt());
        Assert.assertNull(node1.getEventStatus());
        Assert.assertEquals("LATE_REPORTED", node2.getEventStatus());
        Assert.assertEquals("2020-08-12T22:00:00Z", node2.getPlannedAt());
        Assert.assertEquals("PLANNED", node3.getEventStatus());
        Assert.assertNull(node3.getPlannedAt());

        List<Lane> lanes = flow.getLanes();
        Lane lane1 = lanes.get(0);
        Lane lane2 = lanes.get(2);
        Lane lane3 = lanes.get(lanes.size() - 1);

        Assert.assertEquals(14, lanes.size());

        Assert.assertEquals("Packing", lane1.getEventType());
        Assert.assertEquals(new Integer(0), lane1.getPosition());
        Assert.assertEquals("Arrival", lane2.getEventType());
        Assert.assertEquals("C&M Company", lane2.getLocationDescription());
        Assert.assertEquals(new Integer(2), lane2.getPosition());
        Assert.assertEquals("DeliveryItemPOD", lane3.getEventType());
        Assert.assertEquals("C&M Company", lane3.getLocationDescription());
        Assert.assertEquals(new Integer(13), lane3.getPosition());

    }

    @Test
    public void testGetEventReportHistory() {
        String json = SOFUtils.getStringFromResource("/odata/execution-flow-event-history.json");
        Mockito.when(client.readEntitySetAll(contains("/ProcessEventDirectory?$filter=plannedEvent_id"), eq(ProcessEventDirectory.class))).thenReturn(ODataUtils.readEntitySet(json, ProcessEventDirectory.class));

        List<EventEx> eventHistory = executionFlowService.getEventReportHistory(UUID.fromString("4dd9275a-ee76-11ea-829a-5bf1fbadcdd1"));
        Assert.assertEquals(2, eventHistory.size());

        EventEx event1 = eventHistory.get(0);
        EventEx event2 = eventHistory.get(1);

        Assert.assertEquals("2021-09-03T22:17:48Z", event1.getActualAt());
        Assert.assertEquals("Arrival", event1.getEventType());
        Assert.assertEquals("LBN#10010001016", event1.getSenderPartyId());
        Assert.assertEquals("2021-09-03T22:17:47Z", event2.getActualAt());
        Assert.assertEquals("Arrival", event2.getEventType());
        Assert.assertEquals("LBN#10010001016", event2.getSenderPartyId());

    }

}
