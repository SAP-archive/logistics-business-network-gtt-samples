package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CurrentLocationConverterTest {

    @Mock
    private EventService eventService;
    @InjectMocks
    private CurrentLocationConverter currentLocationConverter;

    @Test
    void fromActualSpot_givenActualSpot_shouldConvertToCurrentLocation() {
        // given
        final ActualSpot actualSpot = new ActualSpot();
        final Event event = new Event();
        event.setId(UUID.randomUUID());
        actualSpot.setEvent(event);

        when(eventService.getById(any(), any())).thenReturn(Optional.of(new Event()));

        // when
        final CurrentLocation currentLocation = currentLocationConverter.fromActualSpot(actualSpot);

        // then
        assertThat(currentLocation).isNotNull();
    }
}
