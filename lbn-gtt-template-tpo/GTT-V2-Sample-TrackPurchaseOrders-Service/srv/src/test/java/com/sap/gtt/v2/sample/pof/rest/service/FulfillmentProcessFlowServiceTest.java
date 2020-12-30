package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.configuration.MilestonePopulateExecutorConfiguration;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow.FulfillmentProcessFlowService;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.boot.test.context.SpringBootTest.WebEnvironment.NONE;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = {RestServiceTestConfiguration.class, MilestonePopulateExecutorConfiguration.class}, webEnvironment = NONE)
@ActiveProfiles("test")
public class FulfillmentProcessFlowServiceTest {

    private static final UUID PURCHASE_ORDER_ITEM_ID = UUID.fromString("fbb26df3-1a81-11eb-b5f4-9561ca468fff");
    private static final int CONFIRMATION_LANE_INDEX = 1;

    @Autowired
    private RestTemplate mockRestTemplate;

    @Autowired
    private FulfillmentProcessFlowService fulfillmentProcessFlowService;

    @Test
    public void testGeneratePartialConfirmedFulfillmentProcessFlow() {
        mockRestTemplateResponse("/PurchaseOrderItem", "odata/fulfillment-process-purchase-order-item.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");
        mockRestTemplateResponse("/ConfirmationEvent", "odata/fulfillment-process-confirmation-events.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/fulfillment-process-goods-receipt.json");
        mockRestTemplateResponse("/PlannedEvents", "odata/fulfillment-process-item-planned-events.json");

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(PURCHASE_ORDER_ITEM_ID);

        assertEquals(3, flow.getLanes().size());
        assertTrue(flow.getLanes().stream().allMatch(lane -> lane.getTotal().intValue() == 22));
        assertEquals("20.00", flow.getLanes().get(CONFIRMATION_LANE_INDEX).getCount().toString());
    }

    @Test
    public void testGenerateFulfillmentProcessFlowWithNotPODRelevantDeliveries() {
        mockRestTemplateResponse("/PurchaseOrderItem", "odata/fulfillment-process-purchase-order-item-delivery-items.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");
        mockRestTemplateResponse("/ConfirmationEvent", "odata/fulfillment-process-confirmation-events.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/fulfillment-process-goods-receipt.json");
        mockRestTemplateResponse("/PlannedEvent", "odata/fulfillment-process-delivery_item-planned-events-not-relevant.json");

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(PURCHASE_ORDER_ITEM_ID);

        assertEquals(4, flow.getLanes().size());
        assertTrue(flow.getLanes().stream().allMatch(lane -> lane.getTotal().intValue() == 200));
        // todo add more assertions
    }

    @Test
    public void testGenerateFulfillmentProcessFlowWithPODReportedDeliveries() {
        mockRestTemplateResponse("/PurchaseOrderItem", "odata/fulfillment-process-purchase-order-item-delivery-items.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");
        mockRestTemplateResponse("/ConfirmationEvent", "odata/fulfillment-process-confirmation-events.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/fulfillment-process-goods-receipt.json");
        mockRestTemplateResponse("/PlannedEvent", "odata/fulfillment-process-delivery_item-planned-events-pod-reported.json");

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(PURCHASE_ORDER_ITEM_ID);

        assertEquals(5, flow.getLanes().size());
        assertTrue(flow.getLanes().stream().allMatch(lane -> lane.getTotal().intValue() == 200));
        assertEquals(BigDecimal.valueOf(173), flow.getLanes().get(4).getCount().stripTrailingZeros());
    }

    @Test
    public void testGenerateDeletionFulfillmentProcessFlow() {
        mockRestTemplateResponse("/PurchaseOrderItem", "odata/fulfillment-process-purchase-order-item.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/fulfillment-process-deletion-event.json");
        mockRestTemplateResponse("/ConfirmationEvent", "odata/fulfillment-process-confirmation-events.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/fulfillment-process-goods-receipt.json");

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(PURCHASE_ORDER_ITEM_ID);

        assertEquals(2, flow.getLanes().size());
        assertTrue(flow.getLanes().stream().allMatch(lane -> lane.getTotal().intValue() == 22));
        assertTrue(flow.getLanes().stream().allMatch(lane -> lane.getCount().intValue() == 22));
    }

    private void mockRestTemplateResponse(String urlPart, String responseResourcePath) {
        String resourceString = POFUtils.getStringFromResource(responseResourcePath);
        String url = urlPart.startsWith("/") ? urlPart : "/" + urlPart;
        when(mockRestTemplate.exchange(contains(url), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(ResponseEntity.ok(resourceString));
    }
}