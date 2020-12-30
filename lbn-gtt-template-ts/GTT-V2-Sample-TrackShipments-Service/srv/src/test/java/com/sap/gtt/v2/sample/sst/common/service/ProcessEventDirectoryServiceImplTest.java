package com.sap.gtt.v2.sample.sst.common.service;

import static com.sap.gtt.v2.sample.sst.common.utils.ODataUtils.readEntitySet;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.Objects.nonNull;
import static java.util.UUID.randomUUID;
import static java.util.stream.Collectors.toList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpMethod.GET;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

@ExtendWith(MockitoExtension.class)
class ProcessEventDirectoryServiceImplTest {

    @Mock
    private RestTemplate restTemplate;
    @Spy
    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private ProcessEventDirectoryServiceImpl processEventDirectoryService;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(processEventDirectoryService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    void getByShipmentId_givenShipmentId_shouldReturnProcessEventDirectories() {
        // given
        final String shipmentId = randomUUID().toString();
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");

        when(restTemplate.exchange(contains("/ProcessEventDirectory"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(processEventDirectoriesJson));

        // when
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByShipmentId(shipmentId);

        // then
        assertThat(processEventDirectories).isNotEmpty();
    }

    @Test
    void getActualEventsByPlannedEvents_givenProcessEventDirectories_shouldReturnGroupedEvents() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final ODataResultList<ProcessEventDirectory> resultList = readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class);
        final List<ProcessEventDirectory> processEventDirectories = resultList.getResults();
        final List<ProcessEventDirectory> correlatedProcessEventDirectories = resultList.getResults().stream()
                .filter(processEventDirectory -> nonNull(processEventDirectory.getPlannedEventId()))
                .collect(toList());
        final ODataResultList<ProcessEventDirectory> correlatedResultList = new ODataResultList<>();
        correlatedResultList.setResults(correlatedProcessEventDirectories);

        doReturn(correlatedResultList).when(gttCoreServiceClient).readEntitySetAll(anyString(), eq(ProcessEventDirectory.class));

        // when
        final Map<ProcessEventDirectory, List<ProcessEventDirectory>> actualEventsByPlannedEvents =
                processEventDirectoryService.getActualEventsByPlannedEvents(processEventDirectories);

        // then
        assertThat(actualEventsByPlannedEvents).isNotEmpty();
    }

    @Test
    void getWithoutPlannedEvent_givenProcessEventDirectories_returnWithoutPlannedEvent() {
        // given
        final String processEventDirectoriesJson = getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        // when
        final List<ProcessEventDirectory> withoutPlannedEvent = processEventDirectoryService.getWithoutPlannedEvent(processEventDirectories);

        // then
        assertThat(withoutPlannedEvent).isNotEmpty()
                .extracting(ProcessEventDirectory::getPlannedEventId)
                .containsNull();
    }
}
