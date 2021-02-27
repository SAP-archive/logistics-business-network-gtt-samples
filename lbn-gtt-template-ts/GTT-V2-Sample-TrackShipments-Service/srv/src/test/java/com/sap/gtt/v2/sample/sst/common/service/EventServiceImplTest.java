package com.sap.gtt.v2.sample.sst.common.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;

import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
class EventServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private EventServiceImpl eventService;

    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(gttCoreServiceClient, "techUser", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "criticalInfo", "");
        ReflectionTestUtils.setField(gttCoreServiceClient, "gttBaseUrl", "https://dummy");
        ReflectionTestUtils.setField(eventService, "gttCoreServiceClient", gttCoreServiceClient);
    }

    @Test
    void getById_givenEventsIdAndType_shouldReturnEvent() {
        // given
        final String eventId = randomUUID().toString();
        final String eventType = "Arrival";
        final String arrivalsJson = getStringFromResource("/odata/arrival-events.json");
        final ODataResultList<Event> eventODataResultList = ODataUtils.readEntitySet(arrivalsJson, Event.class);

        when(gttCoreServiceClient.readEntitySetAll(anyString(), eq(Event.class))).thenReturn(eventODataResultList);

        // when
        final Optional<Event> eventOpt = eventService.getById(eventId, eventType);

        // then
        assertThat(eventOpt).isPresent();
    }

    @Test
    void getByEventType_givenEventIdsAndEventType_shouldReturnEvents() {
        // given
        final List<UUID> eventIds = singletonList(randomUUID());
        final String eventType = "Arrival";
        final String arrivalsJson = getStringFromResource("/odata/arrival-events.json");
        final ODataResultList<Event> eventODataResultList = ODataUtils.readEntitySet(arrivalsJson, Event.class);

        when(gttCoreServiceClient.readEntitySetAll(anyString(), eq(Event.class))).thenReturn(eventODataResultList);

        // when
        final List<Event> events = eventService.getByEventType(eventType, eventIds);

        // then
        assertThat(events).isNotEmpty();
    }

    @Test
    void create_givenJsonAndEventType_shouldCreateEvent() {
        // given
        final String json = "{\"key\": \"value\"}";
        final String eventType = "Arrival";

        // when-then
        assertDoesNotThrow(() -> eventService.create(json, eventType));
    }

    @Test
    void getCodeList_givenCodeListNameAndLocale_shouldReturnCodeList() {
        // given
        final String codeListName = "TransportMeansStandardCode";
        final Locale locale = new Locale("en-US");

        when(gttCoreServiceClient.getCodeList(codeListName, locale)).thenReturn(emptyList());

        // when-then
        assertDoesNotThrow(() -> eventService.getCodeList(codeListName, locale));
    }
}
