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
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.rest.model.converter.ShipmentStopsForVpConverter;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.MultiValueMap;

@ExtendWith(MockitoExtension.class)
class ShipmentStopsForVpServiceTest {

    @Mock
    private ShipmentService shipmentService;
    @Mock
    private ShipmentStopsForVpConverter shipmentStopsForVpConverter;
    @InjectMocks
    private ShipmentStopsForVpService shipmentStopsForVpService;

    @Test
    void getAll_givenShipmentId_shouldReturnStopsForVp() {
        // given
        final String shipmentId = randomUUID().toString();
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        when(shipmentService.getById(eq(shipmentId), any(MultiValueMap.class))).thenReturn(Optional.of(shipment));

        // when
        shipmentStopsForVpService.getAll(shipmentId);

        // then
        verify(shipmentStopsForVpConverter, times(1)).fromShipment(any(Shipment.class));
    }

    @Test
    void getAll_givenWrongShipmentId_shouldNotReturnStopsForVp() {
        // given
        final String shipmentId = randomUUID().toString();

        // when
        shipmentStopsForVpService.getAll(shipmentId);

        // then
        verifyNoInteractions(shipmentStopsForVpConverter);
    }
}
