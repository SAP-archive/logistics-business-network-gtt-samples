package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.configuration.local.LocalVcapParser;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.mockito.Mockito;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@TestConfiguration
public class RestServiceTestConfiguration {

    @Bean
    RestTemplate mockRestTemplate() {
        return Mockito.mock(RestTemplate.class);
    }

    @Bean
    GTTCoreServiceClient gttCoreServiceClient(RestTemplate restTemplate) {
        GTTCoreServiceClient gttCoreServiceClient = new GTTCoreServiceClient();
        ReflectionTestUtils.setField(gttCoreServiceClient, "restTemplate", restTemplate);
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        return gttCoreServiceClient;
    }

    @Bean
    LocalVcapParser localVcapParser() { return new LocalVcapParser(); }
}
