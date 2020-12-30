package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.rest.service.forward.ForwardConverter;
import com.sap.gtt.v2.sample.pof.rest.service.forward.ForwardService;
import com.sap.gtt.v2.sample.pof.rest.service.forward.InternalCompleteValueService;
import com.sap.gtt.v2.sample.pof.rest.service.forward.InternalExecutionStatusService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atMostOnce;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
public class ForwardServiceTest {

    private static final UUID MOCK_PURCHASE_ORDER_ITEM_ID = UUID.fromString("725b0a39-841e-5802-9cc3-1c4e6ba2fa4f");
    private static final UUID MOCK_INBOUND_DELIVERY_ITEM_ID = UUID.fromString("2e2a43a7-1e2d-5068-bd8f-e3bfc44a86be");

    @Mock
    private RestTemplate mockRestTemplate;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    private InternalCompleteValueService completeValueService;

    private InternalExecutionStatusService executionStatusService;

    private ForwardService forwardService;

    @Before
    public void init() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");

        completeValueService = Mockito.spy(new InternalCompleteValueService(gttCoreServiceClient));
        executionStatusService = Mockito.spy(new InternalExecutionStatusService(gttCoreServiceClient));

        forwardService = new ForwardService(gttCoreServiceClient, executionStatusService, completeValueService, new ForwardConverter());
    }

    @Test
    public void testPODRelevantInboundDeliveryItemProcessLevel() {
        mockRestTemplateResponse("/PurchaseOrder", "odata/forward_service-order-with-delivery.json");
        mockRestTemplateResponse("/PlannedEvent", "odata/forward_service-relevant-planned-events.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");

        String payload = POFUtils.getStringFromResource("odata/tracked-process-delivery-item.json");

        verify(completeValueService, atMostOnce()).recalculateCompletionValue(any(PurchaseOrder.class));
        verify(executionStatusService, atMostOnce()).updateForNotPODInboundDelivery(any(InboundDeliveryItem.class));

        forwardService.execute(payload);
    }

    @Test
    public void testNotPODRelevantInboundDeliveryItemProcessLevel() {
        mockRestTemplateResponse("/PurchaseOrder", "odata/forward_service-order-with-delivery.json");
        mockRestTemplateResponse("/PlannedEvent", "odata/forward_service-not-relevant-planned-events.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/forward_service-goods-receipt.json");

        String payload = POFUtils.getStringFromResource("odata/tracked-process-delivery-item.json");

        verify(completeValueService, atMostOnce()).recalculateCompletionValue(any(PurchaseOrder.class));
        verify(executionStatusService, atMostOnce()).updateForNotPODInboundDelivery(any(InboundDeliveryItem.class));

        forwardService.execute(payload);
    }

    @Test
    public void testPurchaseOrderItemProcessLevel() {
        mockRestTemplateResponse("/PurchaseOrder", "odata/forward_service-order-without-delivery.json");
        mockRestTemplateResponse("/ProcessEventDirectory", "odata/empty_response.json");
        mockRestTemplateResponse("/GoodsReceipt", "odata/forward_service-goods-receipt.json");

        String payload = POFUtils.getStringFromResource("odata/tracked-process-order-item.json");

        verify(completeValueService, atMostOnce()).recalculateCompletionValue(any(PurchaseOrder.class));
        verify(executionStatusService, atMostOnce()).updateForNotPODInboundDelivery(any(InboundDeliveryItem.class));

        forwardService.execute(payload);
    }


    private void mockRestTemplateResponse(String urlPart, String responseResourcePath) {
        String resourceString = POFUtils.getStringFromResource(responseResourcePath);
        String url = urlPart.startsWith("/") ? urlPart : "/" + urlPart;
        when(mockRestTemplate.exchange(contains(url), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(ResponseEntity.ok(resourceString));
    }
}
