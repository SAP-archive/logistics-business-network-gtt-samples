package com.sap.gtt.v2.sample.sst.rest.service;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SSTServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private SSTServiceImpl sstService;

    @Test
    void getUiAnnotation_shouldCallAllServices() {
        // when
        sstService.getUiAnnotation();

        // then
        verify(gttCoreServiceClient, times(1)).getUiAnnotation();
    }

    @Test
    void getI18n_givenProperties_shouldCallAllServices() {
        // given
        final String properties = "test";

        // when
        sstService.getI18n(properties);

        // then
        verify(gttCoreServiceClient, times(1)).getI18n(properties);
    }

    @Test
    void getHereMapKey_shouldNotThrowException() {
        // then
        assertDoesNotThrow(() -> sstService.getHereMapKey());
    }
}
