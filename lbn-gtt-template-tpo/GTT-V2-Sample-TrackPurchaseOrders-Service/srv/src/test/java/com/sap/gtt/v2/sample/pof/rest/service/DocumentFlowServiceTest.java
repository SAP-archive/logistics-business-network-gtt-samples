package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.pof.odata.handler.POFLocationODataHandler;
import com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderItemODataHandler;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.pof.rest.service.documentFlow.DocumentFlowConverter;
import com.sap.gtt.v2.sample.pof.rest.service.documentFlow.DocumentFlowService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
public class DocumentFlowServiceTest {

    private static final UUID MOCK_PURCHASE_ORDER_ID = UUID.fromString("b5da4b1e-0fd1-5a8d-a1db-7fa589da5308");
    private static final UUID MOCK_PURCHASE_ORDER_ITEM_ID = UUID.fromString("43524b3c-d67e-5a31-a7ee-b459b3871f84");

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private POFPurchaseOrderItemODataHandler pofPurchaseOrderItemODataHandler;

    @Mock
    private POFLocationODataHandler locationODataHandler;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @InjectMocks
    private DocumentFlowConverter converter;

    @InjectMocks
    private DocumentFlowService documentFlowService;

    @Before
    public void setup() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");

        ReflectionTestUtils.setField(documentFlowService, "gttCoreServiceClient", gttCoreServiceClient);
        ReflectionTestUtils.setField(documentFlowService, "converter", converter);

        String stringFromResource = POFUtils.getStringFromResource("odata/purchaseOrder-purchaseOrderItems.json");
        when(restTemplate.exchange(contains("/PurchaseOrder"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(ResponseEntity.ok(stringFromResource));

        when(locationODataHandler.getLocation(any(String.class))).thenReturn(null);
    }

    @Test
    public void testGenerateDocumentFlow() {
        DocumentFlow flow = documentFlowService.generateDocumentFlow(MOCK_PURCHASE_ORDER_ID);
        assertEquals(DocumentFlowGroupEnum.values().length, flow.getGroups().size());
        assertTrue(flow.getNodes().size() > 0);
        assertTrue(flow.getLines().size() > 0);
        assertEquals("PURCHASE_ORDER", flow.getNodes().get(0).getTrackingIdType());
    }

    @Test
    public void testGenerateDocumentItemFlow() {
        DocumentFlow flow = documentFlowService.generateDocumentItemFlow(MOCK_PURCHASE_ORDER_ID, MOCK_PURCHASE_ORDER_ITEM_ID);
        assertEquals(DocumentFlowGroupEnum.values().length, flow.getGroups().size());
        assertTrue(flow.getNodes().size() > 0);
        assertTrue(flow.getLines().size() > 0);
        assertEquals("PURCHASE_ORDER", flow.getNodes().get(0).getTrackingIdType());
    }
}