package com.sap.gtt.v2.sample.sst.rest.controller;

import com.sap.gtt.v2.sample.sst.common.service.EventService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Locale;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class EventControllerTest {

    @Mock
    private EventService eventService;
    @InjectMocks
    private EventController eventController;

    @Test
    void getEventTypesMetadata_givenEventType_shouldCallAllServices() {
        // given
        final String eventType = "Arrival";

        // when
        eventController.getEventTypesMetadata(eventType);

        // then
        verify(eventService, times(1)).getEventTypesMetadata(eventType);
    }

    @Test
    void getCodeList_givenCodeListNameAndLocale_shouldCallAllServices() {
        // given
        final String codeListName = "TransportMeansStandardCode";

        // when
        eventController.getCodeList(codeListName);

        // then
        verify(eventService, times(1)).getCodeList(eq(codeListName), any(Locale.class));
    }

    @Test
    void create_givenBodyAndEventType_shouldCallAllServices() {
        // given
        final String eventType = "Arrival";
        final String json = "{\"key\": \"value\"}";

        // when
        eventController.create(json, eventType);

        // then
        verify(eventService, times(1)).create(json, eventType);
    }
}
