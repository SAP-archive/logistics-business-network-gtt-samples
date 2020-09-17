package com.sap.gtt.v2.sample.sof.service;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrderItem;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.FulfillmentStatus;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.assertj.core.api.Assertions;
import org.junit.Assert;
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

import java.util.List;
import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.EVENT_STATUS_REPORTED;
import static com.sap.gtt.v2.sample.sof.service.SOFService.TRACKED_PROCESS;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@RunWith(MockitoJUnitRunner.class)
public class SOFServiceTest {

    @Spy
    private SOFService sofService;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @Mock
    private RestTemplate restTemplate;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(sofService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    public void testUpdateCompletedAndLateValues() {
        String json = SOFUtils.getStringFromResource("/odata/sales-order-single.json");
        Mockito.when(restTemplate.exchange(contains("/SalesOrder"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/sales-order-item-single.json");
        Mockito.when(restTemplate.exchange(contains("/SalesOrderItem"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        sofService.updateCompletedAndLateValues("afe8b268-3994-5331-afac-52e669da9b7f");

        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrderItem("), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrder("), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrderItemEvent"), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class));

        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrderEvent"), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class));

        SalesOrder salesOrder = gttCoreServiceClient.readEntity("/SalesOrder('dummy')", SalesOrder.class);
        Assertions.assertThat(salesOrder.getDelayed()).isFalse();
        SalesOrderItem salesOrderItem = gttCoreServiceClient.readEntity("/SalesOrderItem('dummy')", SalesOrderItem.class);
        Assertions.assertThat(salesOrderItem.getDelayed()).isFalse();
    }

    @Test
    public void testExecuteTasks() {
        Mockito.doNothing().when(sofService).updateCompletionAndDelayedQuantities(anyString());
        Mockito.doNothing().when(sofService).updateCompletedAndLateValues(anyString());
        Mockito.doNothing().when(sofService).updateLastActivityOfDeliveryItem(any());
        String payload = SOFUtils.getStringFromResource("/odata/payload-received-shipment-pod.json");
        sofService.executeTasks(payload);

        verify(sofService, times(1)).updateCompletionAndDelayedQuantities(anyString());
        verify(sofService, times(1)).updateCompletedAndLateValues(anyString());
    }

    @Test
    public void testGetCarrierRefDocuments() {
        String json = SOFUtils.getStringFromResource("/odata/delivery-item-carrierRefDocuments.json");
        Mockito.when(restTemplate.exchange(contains("/DeliveryItem(guid'b8b7c6c9-9a3f-5299-9a73-13f21f071d4f')"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        List<CarrierRefDocumentForDeliveryItem> docs = sofService.getCarrierRefDocuments(
                UUID.fromString("b8b7c6c9-9a3f-5299-9a73-13f21f071d4f"));

        System.out.println(docs);
        Assertions.assertThat(docs).hasSize(2);
    }

    @Test
    public void testGetFulfillmentStatus() {
        String json = SOFUtils.getStringFromResource("/odata/event-status.json");
        Mockito.when(restTemplate.exchange(contains("/EventStatus?$inlinecount=allpages"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/planned-events.json");
        Mockito.when(restTemplate.exchange(contains("/PlannedEvent?$filter=process_id%20eq%20guid'f970c946-0089-5488-9b13-6077a1d32b0a'"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        List<FulfillmentStatus> fulfillmentStatusList = sofService.getFulfillmentStatus(UUID.fromString("f970c946-0089-5488-9b13-6077a1d32b0a"));
        System.out.println(fulfillmentStatusList);

        Assertions.assertThat(fulfillmentStatusList).hasSize(4);
        for (FulfillmentStatus fulfillmentStatus : fulfillmentStatusList) {
            if (fulfillmentStatus.getEventStatusCode().equals(EVENT_STATUS_REPORTED)) {
                Assertions.assertThat(fulfillmentStatus.getCount()).isEqualTo(1);
            } else if (fulfillmentStatus.getEventStatusCode().equals("PLANNED")) {
                Assertions.assertThat(fulfillmentStatus.getCount()).isEqualTo(7);
            }
        }
    }

    @Test
    public void testUpdateLastActivityOfDeliveryItem() {

        String json = SOFUtils.getStringFromResource("/odata/delivery-item-pod-with-ped.json");
        Mockito.when(restTemplate.exchange(contains("/Event(guid'ef53695d-f283-11ea-b748-e9091d8ac3d9')"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));
        Mockito.when(restTemplate.exchange(contains("/DeliveryItemPOD(guid'ef53695d-f283-11ea-b748-e9091d8ac3d9')?$expand=eventProcesses/plannedEvent"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/delivery-item-received.json");
        JsonObject jsonObject = JsonParser.parseString(json).getAsJsonObject();
        JsonObject trackedProcess = jsonObject.getAsJsonObject(TRACKED_PROCESS);

        sofService.updateLastActivityOfDeliveryItem(trackedProcess);
    }


    @Test
    public void testUpdateCompletionAndDelayedQuantities() {
        String json = SOFUtils.getStringFromResource("/odata/sales-order-single.json");
        Mockito.when(restTemplate.exchange(contains("/SalesOrder"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/sales-order-item-single.json");
        Mockito.when(restTemplate.exchange(contains("/SalesOrderItem"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        Mockito.when(restTemplate.exchange(anyString(),eq(HttpMethod.POST), any(HttpEntity.class) ,eq(String.class))).thenReturn(
                ResponseEntity.ok().body(null));

        sofService.updateCompletionAndDelayedQuantities("afe8b268-3994-5331-afac-52e669da9b7f");
    }


    @Test
    public void testGetI18n() {
        String i18n = SOFUtils.getStringFromResource("/i18n-dummy.properties");
        String codeList = SOFUtils.getStringFromResource("/odata/i18n-code-list.json");
        Mockito.when(restTemplate.exchange(contains("/i18n.properties"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(i18n));
        Mockito.when(restTemplate.exchange(contains("$expand=localized"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(codeList));

        String response = sofService.getI18n("i18n.properties");
        System.out.println(response);
        Assert.assertNotNull(response);
        Assert.assertTrue(response.contains("CO_ExecutionStatus_NOT_STARTED_NAME=Not Started"));
        Assert.assertTrue(response.contains("CO_ExecutionStatus_IN_TRANSIT_NAME=In Transit"));
        Assert.assertTrue(response.contains("CO_ExecutionStatus_COMPLETED_NAME=Completed"));
    }
    

}