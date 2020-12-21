package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.rest.model.AdmissibleUnplannedEvent;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

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
}
