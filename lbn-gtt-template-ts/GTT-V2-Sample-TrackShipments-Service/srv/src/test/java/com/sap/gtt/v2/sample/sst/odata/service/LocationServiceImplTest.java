package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.ODataUtils.readEntity;
import static com.sap.gtt.v2.sample.sst.common.utils.ODataUtils.readEntitySet;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class LocationServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private LocationServiceImpl locationService;

    @Test
    void getByAltKey_givenAltKey_shouldReturnLocation() {
        // given
        final String altKey = "testAltKey";
        final String locationJson = getStringFromResource("/odata/location.json");
        final Location location = readEntity(locationJson, Location.class);

        when(gttCoreServiceClient.getLocation(altKey)).thenReturn(Optional.of(location));

        // when
        final Optional<Location> locationOpt = locationService.getByAltKey(altKey);

        // then
        assertThat(locationOpt).isPresent();
    }

    @Test
    void getAll_thenReturnAllLocations() {
        // given
        final String locationsJson = getStringFromResource("/odata/locations.json");
        final List<Location> locations = readEntitySet(locationsJson, Location.class).getResults();

        when(gttCoreServiceClient.getLocations()).thenReturn(locations);

        // when
        final List<Location> allLocations = locationService.getAll();

        // then
        assertThat(allLocations).isNotEmpty();
    }

    @Test
    void getAll_givenUri_shouldReturnAllLocations() {
        // given
        final String uri = "/Location";
        final String locationsJson = getStringFromResource("/odata/locations.json");
        final ODataResultList<Location> resultList = readEntitySet(locationsJson, Location.class);

        when(gttCoreServiceClient.getLocationsByUri(uri)).thenReturn(resultList);

        // when
        final List<Location> allLocations = locationService.getAll(uri).getResults();

        // then
        assertThat(allLocations).isNotEmpty();
    }

    @Test
    void get_givenUri_shouldReturnLocation() {
        // given
        final String uri = "/Location";
        final String locationJson = getStringFromResource("/odata/location.json");
        final Location location = readEntity(locationJson, Location.class);

        when(gttCoreServiceClient.getLocationByUri(uri)).thenReturn(location);

        // when
        final Location result = locationService.get(uri);

        // then
        assertThat(result).isNotNull();
    }
}
