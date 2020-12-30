package com.sap.gtt.v2.sample.sst.rest.controller;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.ModelService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ModelControllerTest {

    @Mock
    private ModelService modelService;
    @InjectMocks
    private ModelController modelController;

    @Test
    void getUnplannedEventTypesOfTp_givenTrackedProcess_shouldCallAllServices() {
        // given
        final String trackedProcess = "Shipment";

        // when
        modelController.getUnplannedEventTypesOfTp(trackedProcess);

        // then
        verify(modelService, times(1)).getUnplannedEventTypesOfTp(trackedProcess);
    }
}
