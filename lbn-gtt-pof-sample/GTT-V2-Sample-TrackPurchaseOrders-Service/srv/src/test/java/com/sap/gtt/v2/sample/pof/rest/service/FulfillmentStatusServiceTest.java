package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem.FulfillmentStatus;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
public class FulfillmentStatusServiceTest {

    private static final UUID MOCK_INBOUND_DELIVERY_ITEM_ID = UUID.fromString("a5cbef80-7801-55d3-be5c-929160d5b01d");

    @Mock
    private RestTemplate mockRestTemplate;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    private FulfillmentStatusService fulfillmentStatusService;

    @Before
    public void init() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");

        fulfillmentStatusService = new FulfillmentStatusService(gttCoreServiceClient);

        mockRestTemplateResponse("/EventStatus", "odata/event-status.json");
        mockRestTemplateResponse("/PlannedEvent", "odata/fulfillment-status-planned-events.json");
    }

    @Test
    public void testFulfillmentStatus() {
        List<FulfillmentStatus> fulfillmentStatus = fulfillmentStatusService.getFulfillmentStatus(MOCK_INBOUND_DELIVERY_ITEM_ID);

        Assertions.assertEquals(4, fulfillmentStatus.size());
        Assertions.assertEquals(0, getCountByStatusName(fulfillmentStatus, "DELAYED"));
        Assertions.assertEquals(1, getCountByStatusName(fulfillmentStatus, "REPORTED"));
        Assertions.assertEquals(1, getCountByStatusName(fulfillmentStatus, "OVERDUE"));
        Assertions.assertEquals(5, getCountByStatusName(fulfillmentStatus, "PLANNED"));
    }

    private Integer getCountByStatusName(List<FulfillmentStatus> fulfillmentStatuses, String name) {
        return fulfillmentStatuses.stream()
                .filter(it -> it.getEventStatusCode().equals(name))
                .findFirst()
                .map(FulfillmentStatus::getCount)
                .orElse(-1);
    }

    private void mockRestTemplateResponse(String urlPart, String responseResourcePath) {
        String resourceString = POFUtils.getStringFromResource(responseResourcePath);
        String url = urlPart.startsWith("/") ? urlPart : "/" + urlPart;
        when(mockRestTemplate.exchange(contains(url), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(ResponseEntity.ok(resourceString));
    }
}