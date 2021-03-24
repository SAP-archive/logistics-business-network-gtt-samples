package com.sap.gtt.v2.sample.pof.service.client;


import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;

import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.exception.LocationServiceException;
import com.sap.gtt.v2.sample.pof.exception.MetadataServiceException;
import com.sap.gtt.v2.sample.pof.exception.ReadServiceException;
import com.sap.gtt.v2.sample.pof.exception.WriteServiceException;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.TrackedProcess;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

@RunWith(MockitoJUnitRunner.class)
public class GTTCoreServiceClientTest {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");


        String json = POFUtils.getStringFromResource("/odata/tracked-processes.json");
        Mockito.when(restTemplate.exchange(contains("/TrackedProcess"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = POFUtils.getStringFromResource("/odata/tracked-process-single.json");
        Mockito.when(restTemplate.exchange(contains("/TrackedProcess(guid'10106f8dd3-b2e0-582e-ae1b-10d7d851c030d')"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        Mockito.when(restTemplate.exchange(contains("/TrackedProcess/$count"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body("2"));

        Mockito.when(restTemplate.exchange(contains("/SalesOrderItemEvent"), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(null));

    }

    @Test
    public void testQuery() {
        String json = POFUtils.getStringFromResource("/odata/tracked-processes.json");
        String res = gttCoreServiceClient.query("/TrackedProcess");
        Assertions.assertThat(res).isEqualTo(json);

        Mockito.verify(restTemplate, times(1)).exchange(contains("/TrackedProcess"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));
    }

    @Test(expected = ReadServiceException.class)
    public void testQueryShouldThrowException() {
        String uri = "/PurchaseOrder";
        Mockito.when(restTemplate.exchange(contains(uri), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        gttCoreServiceClient.query(uri);
    }

    @Test
    public void testWrite() {
        String json = POFUtils.getStringFromResource("/odata/sales-order-items.json");
        gttCoreServiceClient.write(json, "/SalesOrderItemEvent");
        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrderItemEvent"), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class));
    }

    @Test(expected = WriteServiceException.class)
    public void testWriteShouldThrowException() {
        String uri = "/PurchaseOrder";
        Mockito.when(restTemplate.exchange(contains(uri), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class)))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        gttCoreServiceClient.write(EMPTY, uri);
    }

    @Test
    public void testReadEntitySet() {
        ODataResultList<TrackedProcess> res = gttCoreServiceClient.readEntitySet("/TrackedProcess", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        Assertions.assertThat(res.getResults()).hasSize(2);
    }

    @Test
    public void testReadEntity() {
        TrackedProcess res = gttCoreServiceClient.readEntity("/TrackedProcess(guid'10106f8dd3-b2e0-582e-ae1b-10d7d851c030d')", TrackedProcess.class);
        Assertions.assertThat(res.getId().toString()).isEqualTo("996f8dd3-b2e0-582e-ae1b-9d7d851c030d");
        Assertions.assertThat(res.getPlannedEvents()).hasSize(4);
    }

    @Test
    public void testCountEntitySet() {
        Integer count = gttCoreServiceClient.countEntitySet("/TrackedProcess/$count");
        Assertions.assertThat(count).isEqualTo(2);
    }

    @Test
    public void testGetUiAnnotation() {
        Mockito.when(restTemplate.exchange(contains("/annotation"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body("<?xml version=\"1.0\" encoding=\"utf-8\"?>"));

        String uiAnnotation = gttCoreServiceClient.getUiAnnotation();
        Assertions.assertThat(uiAnnotation).isNotNull();
    }

    @Test
    public void testGetI18n() {
        Mockito.when(restTemplate.exchange(contains("/i18n"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body("com.sap.gtt.core.DisplayFields=Display Fields"));

        String i18n = gttCoreServiceClient.getI18n("i18n.properties");
        Assertions.assertThat(i18n).isEqualTo("com.sap.gtt.core.DisplayFields=Display Fields");
    }

    @Test(expected = MetadataServiceException.class)
    public void testGetI18nError() {
        Mockito.when(restTemplate.exchange(contains("/i18n"), eq(HttpMethod.GET),
            any(HttpEntity.class), eq(String.class)))
            .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));
        gttCoreServiceClient.getI18n("i18n.properties");

    }

    @Test
    public void testGetLocation() {
        Mockito.when(restTemplate.exchange(contains("/Location"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(POFUtils.getStringFromResource("/odata/location.json")));

        String locationAltKey = "xri://sap.com/id:LBN#10013165:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";
        Location location = gttCoreServiceClient.getLocation(locationAltKey);

        Assertions.assertThat(location.getCityName()).isEqualTo("Des Moines");

        /*Assertions.assertThat(location.getLongitudeStr()).isEqualTo("12.12345000");
        Assertions.assertThat(location.getLatitudeStr()).isEqualTo("21.12345000");*/

        Assertions.assertThat(location.getLongitude()).isEqualTo(new BigDecimal("12.12345000"));
        Assertions.assertThat(location.getLatitude()).isEqualTo(new BigDecimal("21.12345000"));

        Assertions.assertThat(location.getFormattedAddress()).isEqualTo("Created by CoreEngine: {{ExternalId_CoreEngine}}$No.10 TIanze road$Des Moines Iowa 100000$United States");
    }
    @Test(expected = LocationServiceException.class)
    public void testGetLocationError() {
        Mockito.when(restTemplate.exchange(contains("/Location"), eq(HttpMethod.GET),
            any(HttpEntity.class), eq(String.class)))
            .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));
        gttCoreServiceClient.getLocation("locationAltKey");
    }
    @Test
    public void testGetLocations() {
        Mockito.when(restTemplate.exchange(contains("/Location"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(POFUtils.getStringFromResource("/odata/location.json")));

        String locationAltKey = "xri://sap.com/id:LBN#10013165:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";
        List<Location> locations = gttCoreServiceClient.getLocations(new HashSet<>(Collections.singletonList(locationAltKey)));
        Location location = locations.get(0);

        Assert.assertEquals(1, locations.size());

        Assertions.assertThat(location.getCityName()).isEqualTo("Des Moines");

        /*Assertions.assertThat(location.getLongitudeStr()).isEqualTo("12.12345000");
        Assertions.assertThat(location.getLatitudeStr()).isEqualTo("21.12345000");*/

        Assertions.assertThat(location.getLongitude()).isEqualTo(new BigDecimal("12.12345000"));
        Assertions.assertThat(location.getLatitude()).isEqualTo(new BigDecimal("21.12345000"));

        Assertions.assertThat(location.getFormattedAddress()).isEqualTo("Created by CoreEngine: {{ExternalId_CoreEngine}}$No.10 TIanze road$Des Moines Iowa 100000$United States");
    }

    @Test
    public void testReadEntitySetAll() {
        ODataResultList<TrackedProcess> res = gttCoreServiceClient.readEntitySetAll("/TrackedProcess", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        Assertions.assertThat(res.getResults()).hasSize(10);
        Mockito.verify(restTemplate, times(5)).exchange(contains("/TrackedProcess"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));

        res = gttCoreServiceClient.readEntitySetAll("/TrackedProcess?$inlinecount=allpages", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        Assertions.assertThat(res.getResults()).hasSize(10);

        res = gttCoreServiceClient.readEntitySetAll("/TrackedProcess?$inlinecount=none", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        Assertions.assertThat(res.getResults()).hasSize(10);

        res = gttCoreServiceClient.readEntitySetAll("/TrackedProcess?" +
                "$filter=altKey eq 'xri://sap.com/id:LBN#10010001006:QM7CLNT910:OUTBOUND_DELIVERY:0080037614'", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(10);
        Assertions.assertThat(res.getResults()).hasSize(10);
    }
}