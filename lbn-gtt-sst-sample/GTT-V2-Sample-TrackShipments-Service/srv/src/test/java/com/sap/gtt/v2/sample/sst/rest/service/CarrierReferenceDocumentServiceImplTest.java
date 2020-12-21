package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.service.ShipmentService;
import com.sap.gtt.v2.sample.sst.rest.model.converter.CarrierReferenceDocumentConverter;
import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.MultiValueMap;

@ExtendWith(MockitoExtension.class)
class CarrierReferenceDocumentServiceImplTest {

    @Mock
    private ShipmentService shipmentService;
    @Mock
    private CarrierReferenceDocumentConverter carrierReferenceDocumentConverter;
    @InjectMocks
    private CarrierReferenceDocumentServiceImpl carrierReferenceDocumentService;

    @Test
    void getByShipmentId_givenShipmentId_shouldReturnCarrierReferenceDocuments() {
        // given
        final String shipmentId = randomUUID().toString();
        final String shipmentJson = SSTUtils.getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        when(shipmentService.getById(anyString(), any(MultiValueMap.class))).thenReturn(Optional.of(shipment));
        when(carrierReferenceDocumentConverter.toDtoList(eq(shipment.getCarrierRefDocuments())))
                .thenReturn(singletonList(new CarrierReferenceDocumentDto()));

        // when
        final List<CarrierReferenceDocumentDto> result = carrierReferenceDocumentService.getByShipmentId(shipmentId);

        // then
        verify(shipmentService, times(1)).getById(anyString(), any(MultiValueMap.class));
        verify(carrierReferenceDocumentConverter, times(1)).toDtoList(shipment.getCarrierRefDocuments());
    }
}
