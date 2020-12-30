package com.sap.gtt.v2.sample.sst.rest.controller;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.rest.service.SSTService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SSTControllerTest {

    @Mock
    private SSTService sstService;
    @InjectMocks
    private SSTController sstController;

    @Test
    void getUiAnnotation_shouldCallAllServices() {
        // when
        sstController.getUiAnnotation();

        // then
        verify(sstService, times(1)).getUiAnnotation();
    }

    @Test
    void getI18n_givenProperties_shouldCallAllServices() {
        // given
        final String properties = "test";

        // when
        sstController.getI18n(properties);

        // then
        verify(sstService, times(1)).getI18n(properties);
    }

    @Test
    void getHereMapKey_shouldCallAllServices() {
        // when
        sstController.getHereMapKey();

        // then
        verify(sstService, times(1)).getHereMapKey();
    }
}
