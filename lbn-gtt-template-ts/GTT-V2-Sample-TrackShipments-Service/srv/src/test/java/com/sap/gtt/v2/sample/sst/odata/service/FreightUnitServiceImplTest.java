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
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
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
class FreightUnitServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @Mock
    private LocationService locationService;
    @InjectMocks
    private FreightUnitServiceImpl freightUnitService;

    @Test
    void getByUri_givenUriWithLocations_shouldReturnFreightUnitWithArrivalAndDepartureLocations() {
        // given
        final String uri = "/FreightUnit?$expand=arrivalLocation,departureLocation";
        final String freightUnitJson = getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);
        final String locationJson = getStringFromResource("/odata/location.json");
        final Location location = ODataUtils.readEntity(locationJson, Location.class);

        when(gttCoreServiceClient.readEntity(contains("/FreightUnit"), eq(FreightUnit.class))).thenReturn(freightUnit);
        when(locationService.getByAltKey(anyString())).thenReturn(Optional.of(location));

        // when
        final Optional<FreightUnit> freightUnitOpt = freightUnitService.getByUri(uri);

        // then
        assertThat(freightUnitOpt).isPresent();
        assertThat(freightUnitOpt).get().extracting(FreightUnit::getArrivalLocation).isNotNull();
        assertThat(freightUnitOpt).get().extracting(FreightUnit::getDepartureLocation).isNotNull();
    }

    @Test
    void getByUri_givenUriWithoutLocations_shouldReturnFreightUnitWithoutArrivalAndDepartureLocations() {
        // given
        final String uri = "/FreightUnit";
        final String freightUnitJson = getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);

        when(gttCoreServiceClient.readEntity(contains("/FreightUnit"), eq(FreightUnit.class))).thenReturn(freightUnit);

        // when
        final Optional<FreightUnit> freightUnitOpt = freightUnitService.getByUri(uri);

        // then
        assertThat(freightUnitOpt).isPresent();
        assertThat(freightUnitOpt).get().extracting(FreightUnit::getArrivalLocation).isNull();
        assertThat(freightUnitOpt).get().extracting(FreightUnit::getDepartureLocation).isNull();
    }

    @Test
    void getAllByUri_givenUri_shouldReturnAllFreightUnits() {
        // given
        final String uri = "/FreightUnit?$expand=arrivalLocation,departureLocation";
        final String freightUnitJson = getStringFromResource("/odata/freight-units.json");
        final ODataResultList<FreightUnit> freightUnitODataResultList = ODataUtils.readEntitySet(freightUnitJson, FreightUnit.class);
        final String locationsJson = getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(gttCoreServiceClient.readEntitySet(contains("/FreightUnit"), eq(FreightUnit.class))).thenReturn(freightUnitODataResultList);
        when(locationService.getAll()).thenReturn(locations);

        // when
        final ODataResultList<FreightUnit> result = freightUnitService.getAllByUri(uri);

        // then
        assertThat(result).isNotNull();
    }

    @Test
    void getById_givenIdWithParams_shouldReturnFreightUnit() {
        // given
        final String id = randomUUID().toString();
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add("key", "value");
        final String freightUnitJson = getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);

        when(gttCoreServiceClient.readEntity(contains("/FreightUnit"), eq(FreightUnit.class))).thenReturn(freightUnit);

        // when
        final Optional<FreightUnit> freightUnitOpt = freightUnitService.getById(id, params);

        // then
        assertThat(freightUnitOpt).isPresent();
    }
}