package com.sap.gtt.v2.sample.sst.service.client;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.model.CodeListValue;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.EventStatus;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Locale;
import java.util.Optional;

import static java.lang.String.format;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.http.HttpMethod.GET;

@ExtendWith(MockitoExtension.class)
class GTTCoreServiceClientTest {

    @Mock
    private RestTemplate restTemplate;
    @InjectMocks
    private GTTCoreServiceClient gttCoreServiceClient;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
    }

    @Test
    void query_givenUri_shouldReturnJsonResult() {
        // given
        final String uri = "/TestEndpoint";
        final String json = "{\"key\": \"value\"}";

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(json));

        // when
        final String resultJson = gttCoreServiceClient.query(uri);

        // then
        assertThat(resultJson).isEqualTo(json);
        verify(restTemplate, times(1))
                .exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class));
    }

    @Test
    void write_givenJsonBody_shouldCallWriteMethod() {
        // given
        final String uri = "/TestEndpoint";
        final String json = "{\"key\": \"value\"}";

        // when
        gttCoreServiceClient.write(json, uri);

        // then
        verify(restTemplate, times(1)).exchange(contains(uri), eq(HttpMethod.POST),
                any(HttpEntity.class), eq(String.class));
    }

    @Test
    void readEntitySet_givenUri_shouldReturnEntitySet() {
        // given
        final String uri = "/EventStatus";
        final String eventStatusesJson = SSTUtils.getStringFromResource("/odata/event-statuses.json");

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(eventStatusesJson));

        // when
        final List<EventStatus> eventStatuses = gttCoreServiceClient.readEntitySet(uri, EventStatus.class).getResults();

        // then
        assertThat(eventStatuses).isNotEmpty();
    }

    @Test
    void readEntity_givenUriWithId_shouldReturnEntity() {
        // given
        final String uri = format("/EventStatus(guid'%s')", randomUUID().toString());
        final String eventStatusesJson = SSTUtils.getStringFromResource("/odata/event-statuses.json");

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(eventStatusesJson));

        // when
        final EventStatus eventStatus = gttCoreServiceClient.readEntity(uri, EventStatus.class);

        // then
        assertThat(eventStatus).isNotNull();
    }

    @Test
    void readEntity_givenUriWithHeaders_shouldReturnEntity() {
        // given
        final String uri = format("/EventStatus(guid'%s')", randomUUID().toString());
        final String eventStatusesJson = SSTUtils.getStringFromResource("/odata/event-statuses.json");
        final HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.setBasicAuth("test", "test");

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(eventStatusesJson));

        // when
        final EventStatus eventStatus = gttCoreServiceClient.readEntity(uri, EventStatus.class, httpHeaders);

        // then
        assertThat(eventStatus).isNotNull();
    }

    @Test
    void countEntitySet_givenUri_shouldReturnCount() {
        // given
        final String uri = "/TestEndpoint/$count";
        final Integer expectedCount = 3;

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(String.valueOf(expectedCount)));

        // when
        final Integer count = gttCoreServiceClient.countEntitySet(uri);

        // then
        assertThat(count).isEqualTo(expectedCount);
    }

    @Test
    void getUiAnnotation_givenAnnotationUri_shouldReturnAnnotation() {
        // given
        final String uri = "/annotation";

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body("<?xml version=\"1.0\" encoding=\"utf-8\"?>"));

        // when
        final String uiAnnotation = gttCoreServiceClient.getUiAnnotation();

        // then
        assertThat(uiAnnotation).isNotNull();
    }

    @Test
    void getI18n_givenUri_ShouldReturnTranslation() {
        // given
        final String uri = "/i18n";

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body("com.sap.gtt.core.DisplayFields=Display Fields"));

        // when
        final String i18n = gttCoreServiceClient.getI18n("i18n.properties");

        // then
        assertThat(i18n).isEqualTo("com.sap.gtt.core.DisplayFields=Display Fields");
    }

    @Test
    void readEntitySetAll_givenUri_shouldReturnEntitySet() {
        // given
        final String uri = "/PlannedEvent?$inlinecount=allpages";
        final String plannedEventJson = SSTUtils.getStringFromResource("/odata/planned-events.json");

        when(restTemplate.exchange(contains(uri), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(plannedEventJson));

        // when
        final ODataResultList<PlannedEvent> result = gttCoreServiceClient.readEntitySetAll(uri, PlannedEvent.class);

        // then
        assertThat(result.getCount()).isNotNull();
        assertThat(result.getResults()).isNotEmpty();
    }

    @Test
    void getLocation_givenLocationAltKey_shouldReturnLocation() {
        // given
        final String altKey = "testAltKey";
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");

        when(restTemplate.exchange(contains("/Location"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(locationsJson));

        // when
        final Optional<Location> locationOpt = gttCoreServiceClient.getLocation(altKey);

        // then
        assertThat(locationOpt).isPresent();
    }

    @Test
    void getLocations_givenLocations_shouldReturnAllLocations() {
        // given
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");

        when(restTemplate.exchange(contains("/Location"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(locationsJson));

        // when
        final List<Location> locations = gttCoreServiceClient.getLocations();

        // then
        assertThat(locations).isNotEmpty();
    }

    @Test
    void getLocationsByUri_givenUri_shouldReturnLocations() {
        // given
        final String uri = "/Location";
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");

        when(restTemplate.exchange(contains("/Location"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(locationsJson));

        // when
        final List<Location> locations = gttCoreServiceClient.getLocationsByUri(uri).getResults();

        // then
        assertThat(locations).isNotEmpty();
    }

    @Test
    void getLocationByUri_givenUri_shouldReturnLocation() {
        // given
        final String uri = "/Location";
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");

        when(restTemplate.exchange(contains("/Location"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(locationsJson));

        // when
        final Location location = gttCoreServiceClient.getLocationByUri(uri);

        // then
        assertThat(location).isNotNull();
    }

    @Test
    void getEventTypesMetadata_givenEventType_shouldReturnMetadata() {
        // given
        final String eventType = "Arrival";

        when(restTemplate.exchange(contains("/metadata"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body("metadata"));

        // when
        final String eventTypesMetadata = gttCoreServiceClient.getEventTypesMetadata(eventType);

        // then
        assertThat(eventTypesMetadata).isNotNull();
    }

    @Test
    void getCodeList_givenCodeListNameAndLocale_shouldReturnCodeList() {
        // given
        final String codeListName = "TransportMeansStandardCode";
        final String codeListsJson = SSTUtils.getStringFromResource("/odata/transport-means-code-list.json");
        final Locale locale = new Locale("en-US");

        when(restTemplate.exchange(contains(codeListName), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(codeListsJson));

        // when
        final List<CodeListValue> codeList = gttCoreServiceClient.getCodeList(codeListName, locale);

        // then
        assertThat(codeList).isNotEmpty();
    }

    @Test
    void getUnplannedEventTypesOfTp_givenTrackedProcess_shouldReturnUnplannedEvents() {
        // given
        final String trackedProcess = "Shipment";
        final String plannedAndUnplannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-and-unplanned-events.json");

        when(restTemplate.exchange(contains("/admissibleEventTypes"), eq(GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(ResponseEntity.ok().body(plannedAndUnplannedEventsJson));

        // when
        final String unplannedEvents = gttCoreServiceClient.getUnplannedEventTypesOfTp(trackedProcess);

        // then
        assertThat(unplannedEvents).isNotNull();
    }
}
