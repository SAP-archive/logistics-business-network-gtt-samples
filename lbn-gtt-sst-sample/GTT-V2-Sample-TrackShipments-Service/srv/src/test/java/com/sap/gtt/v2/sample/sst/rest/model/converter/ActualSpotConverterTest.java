package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import java.util.Collections;
import java.util.List;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ActualSpotConverterTest {

    @Mock
    private LocationService locationService;
    @Mock
    private CoordinatesValidator coordinatesValidator;
    @InjectMocks
    private ActualSpotConverter actualSpotConverter;

    @Test
    void fromProcessEventDirectories_givenProcessEventDirectoriesWithValidActualEventCoordinates_shouldConvertToActualSpots() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(locationService.getAll()).thenReturn(locations);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);

        // when
        final List<ActualSpot> actualSpots = actualSpotConverter.fromProcessEventDirectories(processEventDirectories);

        // then
        assertThat(actualSpots).isNotEmpty();
    }

    @Test
    void fromProcessEventDirectories_givenProcessEventDirectoriesWithValidPlannedEventCoordinates_shouldConvertToActualSpots() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final String locationsJson = SSTUtils.getStringFromResource("/odata/locations.json");
        final List<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class).getResults();

        when(locationService.getAll()).thenReturn(locations);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(false);

        // when
        final List<ActualSpot> actualSpots = actualSpotConverter.fromProcessEventDirectories(processEventDirectories);

        // then
        assertThat(actualSpots).isNotEmpty();
    }
}
