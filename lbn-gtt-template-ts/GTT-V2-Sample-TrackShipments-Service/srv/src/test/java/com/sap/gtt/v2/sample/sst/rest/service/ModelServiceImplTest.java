package com.sap.gtt.v2.sample.sst.rest.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.rest.model.AdmissibleUnplannedEvent;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ModelServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private ModelServiceImpl modelService;

    @Test
    void getUnplannedEventTypesOfTp_givenTrackedProcess_shouldReturnUnplannedEventTypes() {
        // given
        final String trackedProcess = "Shipment";
        final String plannedAndUnplannedEventsJson = SSTUtils.getStringFromResource("/odata/planned-and-unplanned-events.json");

        when(gttCoreServiceClient.getUnplannedEventTypesOfTp(trackedProcess)).thenReturn(plannedAndUnplannedEventsJson);

        // when
        final List<AdmissibleUnplannedEvent> unplannedEventTypesOfTp = modelService.getUnplannedEventTypesOfTp(trackedProcess);

        // then
        assertThat(unplannedEventTypesOfTp).hasSize(25);
    }

    @Test
    void getEventTypesMetadata_givenEventType_shouldReturnEventTypeMetadata() {
        // given
        final String eventType = "Arrival";
        final String trackedProcess = "Shipment";

        when(gttCoreServiceClient.getEventTypesMetadata(trackedProcess, eventType)).thenReturn("metadata");

        // when-then
        assertDoesNotThrow(() -> modelService.getEventTypesMetadata(trackedProcess, eventType));
    }
}
