package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

@ExtendWith(MockitoExtension.class)
class ShipmentServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @Mock
    private LocationService locationService;
    @InjectMocks
    private ShipmentServiceImpl shipmentService;

    @Test
    void getById_givenId_shouldReturnShipment() {
        // given
        final String id = randomUUID().toString();
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        when(gttCoreServiceClient.readEntity(contains("/Shipment"), eq(Shipment.class))).thenReturn(shipment);

        // when
        final Optional<Shipment> shipmentOpt = shipmentService.getById(id);

        // then
        assertThat(shipmentOpt).isPresent();
    }

    @Test
    void getById_givenIdWithParams_shouldReturnShipment() {
        // given
        final String id = randomUUID().toString();
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("key", "value");
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        when(gttCoreServiceClient.readEntity(contains("/Shipment"), eq(Shipment.class))).thenReturn(shipment);

        // when
        final Optional<Shipment> shipmentOpt = shipmentService.getById(id, params);

        // then
        assertThat(shipmentOpt).isPresent();
    }

    @Test
    void getByUri_givenUriWithLocations_shouldReturnShipmentWithArrivalAndDepartureLocations() {
        // given
        final String uri = "/Shipment?$expand=arrivalLocation,departureLocation";
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);
        final String locationJson = getStringFromResource("/odata/location.json");
        final Location location = ODataUtils.readEntity(locationJson, Location.class);

        when(gttCoreServiceClient.readEntity(contains("/Shipment"), eq(Shipment.class))).thenReturn(shipment);
        when(locationService.getByAltKey(anyString())).thenReturn(Optional.of(location));

        // when
        final Optional<Shipment> shipmentOpt = shipmentService.getByUri(uri);

        // then
        assertThat(shipmentOpt).isPresent();
        assertThat(shipmentOpt).get().extracting(Shipment::getArrivalLocation).isNotNull();
        assertThat(shipmentOpt).get().extracting(Shipment::getDepartureLocation).isNotNull();
    }

    @Test
    void getByUri_givenUriWithoutLocations_shouldReturnShipmentWithoutArrivalAndDepartureLocations() {
        // given
        final String uri = "/Shipment";
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        when(gttCoreServiceClient.readEntity(contains("/Shipment"), eq(Shipment.class))).thenReturn(shipment);

        // when
        final Optional<Shipment> shipmentOpt = shipmentService.getByUri(uri);

        // then
        assertThat(shipmentOpt).isPresent();
        assertThat(shipmentOpt).get().extracting(Shipment::getArrivalLocation).isNull();
        assertThat(shipmentOpt).get().extracting(Shipment::getDepartureLocation).isNull();
    }

    @Test
    void getAllByUri_givenUri_shouldReturnAllShipments() {
        // given
        final String uri = "/Shipment?$expand=arrivalLocation,departureLocation";
        final String shipmentsJson = getStringFromResource("/odata/shipments.json");
        final ODataResultList<Shipment> shipmentODataResultList = ODataUtils.readEntitySet(shipmentsJson, Shipment.class);
        final String locationsJson = getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(gttCoreServiceClient.readEntitySet(contains("/Shipment"), eq(Shipment.class))).thenReturn(shipmentODataResultList);
        when(locationService.getAll()).thenReturn(locations);

        // when
        final ODataResultList<Shipment> result = shipmentService.getAllByUri(uri);

        // then
        assertThat(result).isNotNull();
    }
}
