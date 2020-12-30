package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class StopsForVpConverterTest {

    @Mock
    private LocationService locationService;
    @InjectMocks
    private StopsForVpConverter stopsForVpConverter;

    @Test
    void fromShipment_givenShipment_shouldConvertToStopsForVp() {
        // given
        final String shipmentJson = SSTUtils.getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(locationService.getAll()).thenReturn(locations);

        // when
        final List<StopsForVp> stopsForVps = stopsForVpConverter.fromShipment(shipment);

        // then
        assertThat(stopsForVps).isNotEmpty();
    }
}
