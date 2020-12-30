package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpMethod.GET;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.model.EventStatus;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class EventStatusServiceImplTest {

    @Mock
    private RestTemplate restTemplate;
    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private EventStatusServiceImpl eventStatusService;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(eventStatusService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    void getAll_givenEventStatuses_shouldReturnAll() {
        // given
        final String eventStatusesJson = getStringFromResource("/odata/event-statuses.json");

        when(restTemplate.exchange(contains("/EventStatus"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(eventStatusesJson));

        // when
        final List<EventStatus> eventStatuses = eventStatusService.getAll();

        // then
        assertThat(eventStatuses).isNotEmpty();
    }
}
