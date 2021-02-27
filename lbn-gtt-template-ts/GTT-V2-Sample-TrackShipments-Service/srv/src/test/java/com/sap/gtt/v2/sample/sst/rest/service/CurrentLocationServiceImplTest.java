package com.sap.gtt.v2.sample.sst.rest.service;

import static java.math.BigDecimal.ONE;
import static java.util.Collections.singletonList;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.rest.helper.ActualSpotHelper;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.converter.CurrentLocationConverter;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CurrentLocationServiceImplTest {

    @Mock
    private ProcessEventDirectoryService processEventDirectoryService;
    @Mock
    private CoordinatesValidator coordinatesValidator;
    @Mock
    private CurrentLocationConverter currentLocationConverter;
    @Mock
    private ActualSpotHelper actualSpotHelper;
    @InjectMocks
    private CurrentLocationServiceImpl currentLocationService;

    @Test
    void getByShipmentId_givenProcessEventDirectoriesWithInvalidCoordinates_shouldReturnEmptyCurrentLocation() {
        // given
        final String shipmentId = randomUUID().toString();
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();

        when(processEventDirectoryService.getByTrackedProcessId(shipmentId)).thenReturn(processEventDirectories);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(false);
        when(actualSpotHelper.getAllAscending(any())).thenReturn(singletonList(new ActualSpot()));

        // when
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getByTrackedProcessId(shipmentId);

        // then
        assertThat(currentLocationOpt).isNotPresent();
        verify(actualSpotHelper, times(1)).getAllAscending(any());
        verify(coordinatesValidator, times(1)).isValid(any(), any());
        verifyNoInteractions(currentLocationConverter);
    }

    @Test
    void getByShipmentId_givenProcessEventDirectoriesWithValidCoordinates_shouldReturnCurrentLocation() {
        // given
        final String shipmentId = randomUUID().toString();
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLongitude(ONE);
        actualSpot.setLatitude(ONE);

        when(processEventDirectoryService.getByTrackedProcessId(shipmentId)).thenReturn(processEventDirectories);
        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);
        when(actualSpotHelper.getAllAscending(any())).thenReturn(singletonList(actualSpot));
        when(currentLocationConverter.fromActualSpot(any())).thenReturn(new CurrentLocation());

        // when
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getByTrackedProcessId(shipmentId);

        // then
        assertThat(currentLocationOpt).isPresent();
        verify(actualSpotHelper, times(1)).getAllAscending(any());
        verify(coordinatesValidator, times(1)).isValid(any(), any());
        verify(currentLocationConverter, times(1)).fromActualSpot(any());
    }

    @Test
    void getFromActualSpots_givenActualSpotsWithInvalidCoordinates_shouldReturnEmptyCurrentLocation() {
        // given
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLongitude(ONE);
        actualSpot.setLatitude(ONE);

        when(coordinatesValidator.isValid(any(), any())).thenReturn(false);

        // when
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getFromActualSpots(singletonList(actualSpot));

        // then
        assertThat(currentLocationOpt).isNotPresent();
        verify(coordinatesValidator, times(1)).isValid(any(), any());
        verifyNoInteractions(currentLocationConverter);
    }

    @Test
    void getFromActualSpots_givenActualSpots_shouldReturnCurrentLocation() {
        // given
        final ActualSpot actualSpot = new ActualSpot();
        actualSpot.setLongitude(ONE);
        actualSpot.setLatitude(ONE);

        when(coordinatesValidator.isValid(any(), any())).thenReturn(true);
        when(currentLocationConverter.fromActualSpot(any())).thenReturn(new CurrentLocation());

        // when
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getFromActualSpots(singletonList(actualSpot));

        // then
        assertThat(currentLocationOpt).isPresent();
        verify(coordinatesValidator, times(1)).isValid(any(), any());
        verify(currentLocationConverter, times(1)).fromActualSpot(any());
    }
}
