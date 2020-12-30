package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

@RunWith(MockitoJUnitRunner.class)
public class ImpactAnalysisServiceTest {
    @Spy
    private ImpactAnalysisService impactAnalysisService;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @Mock
    private RestTemplate restTemplate;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(impactAnalysisService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    public void testGetInitialNodesStartsFromShipment() {
        String json = SOFUtils.getStringFromResource("/odata/delay-event-processes-shipment.json");
        Mockito.when(restTemplate.exchange(contains("/Delay"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delay-shipment.json");
        Mockito.when(restTemplate.exchange(contains("/Shipment"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delay-deliveries.json");
        Mockito.when(restTemplate.exchange(contains("/Delivery"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        DocumentFlow documentFlow = impactAnalysisService.getInitialNodes(UUID.fromString("c08d25e7-b0cf-5685-b372-a0947cb03597"),
                UUID.fromString("5cc50c5b-dc85-11ea-aea4-fba5690bb239"));

        System.out.println(new Gson().toJson(documentFlow));
        Assertions.assertThat(documentFlow.getNodes()).hasSize(2);
        Assertions.assertThat(documentFlow.getNodes().get(0).getTrackingIdType()).isEqualTo("SHIPMENT_ORDER");
        Assertions.assertThat(documentFlow.getNodes().get(0).getGroup()).isEqualTo(5);

        Assertions.assertThat(documentFlow.getNodes().get(1).getTrackingIdType()).isEqualTo("OUTBOUND_DELIVERY");
        Assertions.assertThat(documentFlow.getNodes().get(1).getGroup()).isEqualTo(4);

        Assertions.assertThat(documentFlow.getLines()).hasSize(1);
        Assertions.assertThat(documentFlow.getLines().get(0).getStatus()).isEqualTo("Error");

        Mockito.verify(restTemplate, times(1)).exchange(contains("/Shipment"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(1)).exchange(contains("/Delivery"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

    }

    @Test
    public void testGetInitialNodesStartsFromDelivery() {
        String json = SOFUtils.getStringFromResource("/odata/delay-event-processes-delivery.json");
        Mockito.when(restTemplate.exchange(contains("/Delay"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delay-deliveries-delivery.json");
        Mockito.when(restTemplate.exchange(contains("/Delivery?$filter="), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delay-single-delivery.json");
        Mockito.when(restTemplate.exchange(contains("/Delivery(guid'f3c9722e-0bef-5b99-bfbb-42cd4210d13e')?$expand=deliveryItemTPs/deliveryItem"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));


        DocumentFlow documentFlow = impactAnalysisService.getInitialNodes(UUID.fromString("01ad6cfd-1cf4-559b-80d1-5a3910bcfb74"),
                UUID.fromString("b874a90a-f3fd-11ea-bbeb-9fbe6ce1a21d"));

        System.out.println(new Gson().toJson(documentFlow));
        Assertions.assertThat(documentFlow.getNodes()).hasSize(3);

        Mockito.verify(restTemplate, times(1)).exchange(contains("/Delay"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(2)).exchange(contains("/Delivery"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));
    }

    @Test
    public void testGetInitialNodesStartsFromDeliveryItem() {
        String json = SOFUtils.getStringFromResource("/odata/delay-event-processes-delivery-item.json");
        Mockito.when(restTemplate.exchange(contains("/Delay"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delay-deliveries-delivery-item-with-salesOrderItem.json");
        Mockito.when(restTemplate.exchange(contains("/DeliveryItem"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        DocumentFlow documentFlow = impactAnalysisService.getInitialNodes(UUID.fromString("d10d4e84-d7f1-55bf-a449-1cff32adaefd"),
                UUID.fromString("092ddc3c-f3d7-11ea-bbeb-09888ad1bb65"));

        System.out.println(new Gson().toJson(documentFlow));
        Assertions.assertThat(documentFlow.getNodes()).hasSize(2);

        Mockito.verify(restTemplate, times(1)).exchange(contains("/Delay"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(2)).exchange(contains("/DeliveryItem"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));
    }
}