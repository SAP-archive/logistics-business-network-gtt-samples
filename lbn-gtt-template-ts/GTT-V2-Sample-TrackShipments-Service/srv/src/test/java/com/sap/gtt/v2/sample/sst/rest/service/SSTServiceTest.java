package com.sap.gtt.v2.sample.sst.rest.service;

import static org.junit.Assert.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.rest.service.SSTServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class SSTServiceTest {

    @Mock
    private RestTemplate restTemplate;

    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @InjectMocks
    private SSTServiceImpl sstService;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(sstService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    void testGetI18n() {
        String i18n = SSTUtils.getStringFromResource("/i18n-dummy.properties");

        when(restTemplate.exchange(contains("/i18n.properties"), eq(HttpMethod.GET),
                any(HttpEntity.class), eq(String.class))).thenReturn(
                ResponseEntity.ok().body(i18n));

        String response = sstService.getI18n("i18n.properties");
        System.out.println(response);

        assertNotNull(response);
    }
}
