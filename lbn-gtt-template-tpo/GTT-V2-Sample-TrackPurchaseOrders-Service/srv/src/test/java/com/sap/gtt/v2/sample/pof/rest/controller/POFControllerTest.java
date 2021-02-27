package com.sap.gtt.v2.sample.pof.rest.controller;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.domain.Route;
import com.sap.gtt.v2.sample.pof.odata.model.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem.FulfillmentStatus;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import com.sap.gtt.v2.sample.pof.rest.service.FulfillmentStatusService;
import com.sap.gtt.v2.sample.pof.rest.service.documentFlow.DocumentFlowService;
import com.sap.gtt.v2.sample.pof.rest.service.forward.ForwardService;
import com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow.FulfillmentProcessFlowService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.TimeTrackingService;
import com.sap.gtt.v2.sample.pof.service.MapService;
import com.sap.gtt.v2.sample.pof.service.POFService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;

@RunWith(SpringRunner.class)
public class POFControllerTest {

    private static final String HERE_MAP_KEY = "HERE_MAP_KEY";
    private static final UUID MOCK_PURCHASE_ORDER_ID = UUID.randomUUID();
    private static final UUID MOCK_PURCHASE_ORDER_ITEM_ID = UUID.randomUUID();
    private static final UUID MOCK_INBOUND_DELIVERY_ITEM_ID = UUID.randomUUID();

    @Mock
    private POFService pofService;

    @Mock
    private DocumentFlowService documentFlowService;

    @Mock
    private FulfillmentProcessFlowService fulfillmentProcessFlowService;

    @Mock
    private TimeTrackingService timeTrackingService;

    @Mock
    private FulfillmentStatusService fulfillmentStatusService;

    @Mock
    private ForwardService forwardService;

    @Mock
    private MapService mapService;

    @InjectMocks
    private POFController pofController;

    @Before
    public void init() {
        ReflectionTestUtils.setField(pofController, "hereMapKey", HERE_MAP_KEY);
    }

    @Test
    public void forwardTest() {
        doNothing().when(forwardService).execute(anyString());
        pofController.forward("Any Body");
    }

    @Test
    public void helloTest() {
        String answer = pofController.hello();
        Assert.assertEquals("hello", answer);
    }

    @Test
    public void getDocumentFlowTest() {
        DocumentFlow expect = getDocumentFlow();
        Mockito.doReturn(expect).when(documentFlowService).generateDocumentFlow(MOCK_PURCHASE_ORDER_ID);
        ResponseEntity<DocumentFlow> actual = pofController.getDocumentFlow(MOCK_PURCHASE_ORDER_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getDocumentItemFlowTest() {
        DocumentFlow expect = getDocumentFlow();
        Mockito.doReturn(expect).when(documentFlowService).generateDocumentItemFlow(MOCK_PURCHASE_ORDER_ID, MOCK_PURCHASE_ORDER_ITEM_ID);
        ResponseEntity<DocumentFlow> actual = pofController.getDocumentItemFlow(MOCK_PURCHASE_ORDER_ID, MOCK_PURCHASE_ORDER_ITEM_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getFulfillmentProcessFlowTest() {
        FulfillmentProcessFlow expect = new FulfillmentProcessFlow() {{
            setLanes(Collections.emptyList());
        }};

        Mockito.doReturn(expect).when(fulfillmentProcessFlowService).generateFulfillmentProcessFlow(MOCK_PURCHASE_ORDER_ITEM_ID);

        ResponseEntity<FulfillmentProcessFlow> actual = pofController.getFulfillmentProcessFlow(MOCK_PURCHASE_ORDER_ITEM_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getFulfillmentStatusTest() {
        List<FulfillmentStatus> expect = Collections.singletonList(
                new FulfillmentStatus("eventStatusCode", 0)
        );
        Mockito.doReturn(expect).when(fulfillmentStatusService).getFulfillmentStatus(MOCK_INBOUND_DELIVERY_ITEM_ID);

        ResponseEntity<List<FulfillmentStatus>> actual = pofController.getFulfillmentStatus(MOCK_INBOUND_DELIVERY_ITEM_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getUiAnnotationTest() {
        String expect = "uiAnnotations";
        Mockito.doReturn(expect).when(pofService).getUiAnnotation();

        String actual = pofController.getUiAnnotation();

        Assert.assertEquals(expect, actual);
    }

    @Test
    public void getI18nTest() {
        String expect = "expectedInternalization";
        Mockito.doReturn(expect).when(pofService).getI18n(anyString());

        String actual = pofController.getI18n(EMPTY);

        Assert.assertEquals(expect, actual);
    }

    @Test
    public void getTimeLineEventTest() {
        List<TimelineEvent> expect = Collections.singletonList(
                new TimelineEvent()
        );
        Mockito.doReturn(expect).when(timeTrackingService).getByDeliveryItemId(MOCK_INBOUND_DELIVERY_ITEM_ID.toString());

        ResponseEntity<List<TimelineEvent>> actual = pofController.getTimeLineEvent(MOCK_INBOUND_DELIVERY_ITEM_ID.toString());

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getHereMapKeyTest() {
        String hereMapKey = pofController.getHereMapKey();

        JsonObject expect = new JsonObject();
        expect.addProperty("key", HERE_MAP_KEY);
        Assert.assertEquals(expect.toString(), hereMapKey);
    }

    @Test
    public void getRoutesTest() {
        List<Route> expect = Collections.singletonList(new Route());

        doReturn(expect).when(mapService).getRoutes(MOCK_INBOUND_DELIVERY_ITEM_ID.toString());
        ResponseEntity<List<Route>> actual = pofController.getRoutes(MOCK_INBOUND_DELIVERY_ITEM_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    @Test
    public void getCarrierRefDocumentsTest() {
        List<CarrierRefDocumentForDeliveryItem> expect = Collections.singletonList(new CarrierRefDocumentForDeliveryItem());

        doReturn(expect).when(pofService).getCarrierRefDocuments(MOCK_INBOUND_DELIVERY_ITEM_ID);
        ResponseEntity<List<CarrierRefDocumentForDeliveryItem>> actual = pofController.getCarrierRefDocuments(MOCK_INBOUND_DELIVERY_ITEM_ID);

        Assert.assertEquals(HttpStatus.OK, actual.getStatusCode());
        Assert.assertEquals(expect, actual.getBody());
    }

    private DocumentFlow getDocumentFlow() {
        return new DocumentFlow() {{
            setGroups(Collections.emptyList());
            setLines(Collections.emptyList());
            setNodes(Collections.emptyList());
        }};
    }
}
