package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.UUID.randomUUID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.rest.model.converter.FreightUnitStopsForVpConverter;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.MultiValueMap;

@ExtendWith(MockitoExtension.class)
class FreightUnitStopsForVpServiceTest {

    @Mock
    private FreightUnitService freightUnitService;
    @Mock
    private FreightUnitStopsForVpConverter freightUnitStopsForVpConverter;
    @InjectMocks
    private FreightUnitStopsForVpService freightUnitStopsForVpService;

    @Test
    void getAll_givenFreightUnitId_shouldReturnStopsForVp() {
        // given
        final String freightUnitId = randomUUID().toString();
        final String freightUnitJson = getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);

        when(freightUnitService.getById(eq(freightUnitId), any(MultiValueMap.class))).thenReturn(Optional.of(freightUnit));

        // when
        freightUnitStopsForVpService.getAll(freightUnitId);

        // then
        verify(freightUnitStopsForVpConverter, times(1)).fromFreightUnit(any(FreightUnit.class));
    }

    @Test
    void getAll_givenWrongFreightUnitId_shouldNotReturnStopsForVp() {
        // given
        final String freightUnitId = randomUUID().toString();

        // when
        freightUnitStopsForVpService.getAll(freightUnitId);

        // then
        verifyNoInteractions(freightUnitStopsForVpConverter);
    }
}