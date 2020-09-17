package com.sap.gtt.v2.sample.sof.service.client;


import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.TrackedProcess;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
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

import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.times;

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


        String json = SOFUtils.getStringFromResource("/odata/tracked-processes.json");
        Mockito.when(restTemplate.exchange(contains("/TrackedProcess"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(json));

        json = SOFUtils.getStringFromResource("/odata/tracked-process-single.json");
        Mockito.when(restTemplate.exchange(contains("/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')"), eq(HttpMethod.GET),
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
        String json = SOFUtils.getStringFromResource("/odata/tracked-processes.json");
        String res = gttCoreServiceClient.query("/TrackedProcess");
        Assertions.assertThat(res).isEqualTo(json);

        Mockito.verify(restTemplate, times(1)).exchange(contains("/TrackedProcess"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class));
    }

    @Test
    public void testWrite() {
        String json = SOFUtils.getStringFromResource("/odata/sales-order-items.json");
        gttCoreServiceClient.write(json, "/SalesOrderItemEvent");
        Mockito.verify(restTemplate, times(1)).exchange(contains("/SalesOrderItemEvent"), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class));
    }
    @Test
    public void testReadEntitySet() {
        ODataResultList<TrackedProcess> res = gttCoreServiceClient.readEntitySet("/TrackedProcess", TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(624);
        Assertions.assertThat(res.getResults()).hasSize(2);
    }

    @Test
    public void testReadEntity() {
        TrackedProcess res = gttCoreServiceClient.readEntity("/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')", TrackedProcess.class);
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

    @Test
    public void testGetLocation() {
        Mockito.when(restTemplate.exchange(contains("/Location"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(SOFUtils.getStringFromResource("/odata/location.json")));

        String locationAltKey = "xri://sap.com/id:LBN#10013165:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";
        Location location = gttCoreServiceClient.getLocation(locationAltKey);

        Assertions.assertThat(location.getCityName()).isEqualTo("Des Moines");

        Assertions.assertThat(location.getLongitudeStr()).isEqualTo("12.12345000");
        Assertions.assertThat(location.getLatitudeStr()).isEqualTo("21.12345000");

        Assertions.assertThat(location.getLongitude()).isEqualTo(new BigDecimal("12.12345000"));
        Assertions.assertThat(location.getLatitude()).isEqualTo(new BigDecimal("21.12345000"));

        Assertions.assertThat(location.getFormattedAddress()).isEqualTo("Created by CoreEngine: {{ExternalId_CoreEngine}}$No.10 TIanze road$Des Moines Iowa 100000$United States");
    }

    @Test
    public void testGetLocations() {
        Mockito.when(restTemplate.exchange(contains("/Location"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(SOFUtils.getStringFromResource("/odata/location.json")));

        String locationAltKey = "xri://sap.com/id:LBN#10013165:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";
        List<Location> locations = gttCoreServiceClient.getLocations(new HashSet<>(Collections.singletonList(locationAltKey)));
        Location location = locations.get(0);

        Assert.assertEquals(1, locations.size());

        Assertions.assertThat(location.getCityName()).isEqualTo("Des Moines");

        Assertions.assertThat(location.getLongitudeStr()).isEqualTo("12.12345000");
        Assertions.assertThat(location.getLatitudeStr()).isEqualTo("21.12345000");

        Assertions.assertThat(location.getLongitude()).isEqualTo(new BigDecimal("12.12345000"));
        Assertions.assertThat(location.getLatitude()).isEqualTo(new BigDecimal("21.12345000"));

        Assertions.assertThat(location.getFormattedAddress()).isEqualTo("Created by CoreEngine: {{ExternalId_CoreEngine}}$No.10 TIanze road$Des Moines Iowa 100000$United States");
    }
}