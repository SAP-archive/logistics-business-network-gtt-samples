package com.sap.gtt.v2.sample.pof.service;

import com.sap.gtt.v2.sample.pof.odata.model.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.assertj.core.api.Assertions;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;

@RunWith(MockitoJUnitRunner.class)
public class POFServiceTest {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    private POFService pofService;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        pofService = Mockito.spy(new POFService(gttCoreServiceClient));
    }

    @Test
    public void testGetI18n() {
        String i18n = POFUtils.getStringFromResource("/i18n-dummy.properties");
        String codeList = POFUtils.getStringFromResource("/odata/i18n-code-list.json");
        Mockito.when(restTemplate.exchange(contains("/i18n.properties"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(i18n));

        Mockito.when(restTemplate.exchange(contains("$expand=localized"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(codeList));

        String response = pofService.getI18n("i18n.properties");
        System.out.println(response);
        Assert.assertNotNull(response);
        Assert.assertTrue(response.contains("com.sap.gtt.core.DisplayFields=Display Fields"));
        Assert.assertTrue(response.contains("EL_SalesOrderItem_uom_LABEL=UoM"));
        Assert.assertTrue(response.contains("EL_POD_estimatedArrival_LABEL=Estimated Arrival"));
    }

    @Test
    public void testGetUiAnnotation() {
        String uiAnnotation = POFUtils.getStringFromResource("/odata/annotations.xml");
        Mockito.when(restTemplate.exchange(contains("/annotation"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok(uiAnnotation));

        String response = pofService.getUiAnnotation();
        System.out.println(response);
        Assert.assertNotNull(response);
        Assert.assertTrue(response.contains("<?xml version=\"1.0\" encoding=\"utf-8\"?>"));
        Assert.assertTrue(response.contains("<Annotation Term=\"Common.Label\" String=\"{@i18n&gt;EL_Event_trackingIdType_LABEL}\"/>"));
        Assert.assertTrue(response.contains("<Annotation Term=\"Common.Label\" String=\"{@i18n&gt;EL_Event_cloneInstanceId_LABEL}\"/>"));
    }

    @Test
    public void testGetCarrierRefDocuments() {
        String json = POFUtils.getStringFromResource("/odata/delivery-item-carrierRefDocuments.json");
        Mockito.when(restTemplate.exchange(contains("/InboundDeliveryItem(guid'b8b7c6c9-9a3f-5299-9a73-13f21f071d4f')"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        List<CarrierRefDocumentForDeliveryItem> docs = pofService.getCarrierRefDocuments(
                UUID.fromString("b8b7c6c9-9a3f-5299-9a73-13f21f071d4f"));

        System.out.println(docs);
        Assertions.assertThat(docs).hasSize(2);
    }
}