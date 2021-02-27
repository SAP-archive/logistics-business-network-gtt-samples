package com.sap.gtt.v2.sample.pof.configuration;

import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;

@RunWith(PowerMockRunner.class)
public class VcapParserTest {

    private static final String DESTINATION_NAME = "test-destination";

    @Mock
    private RestTemplate mockRestTemplate;

    @InjectMocks
    private VcapParser vcapParser;

    private String vcapServices;

    @Before
    public void init() {
        vcapServices = POFUtils.getStringFromResource("conf/vcap_services.txt");
        ReflectionTestUtils.setField(vcapParser, "vcapServices", vcapServices);
    }

    @Test
    public void getDestination() {
        String destinationJson = POFUtils.getStringFromResource("conf/destination.json");
        Mockito.when(mockRestTemplate.exchange(contains(DESTINATION_NAME), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok(destinationJson));
        String authToken = POFUtils.getStringFromResource("conf/access-token.json");
        Mockito.when(mockRestTemplate.exchange(contains("IDENTITY"), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok(authToken));

        Destination destination = vcapParser.getDestination("test-destination");

        Assert.assertEquals("test-destination", destination.getName());
        Assert.assertEquals("test-destination", destination.getDescription());
        Assert.assertEquals("test-id", destination.getClientId());
    }

    @Test
    public void requestTechniqueToken() {
    }
}