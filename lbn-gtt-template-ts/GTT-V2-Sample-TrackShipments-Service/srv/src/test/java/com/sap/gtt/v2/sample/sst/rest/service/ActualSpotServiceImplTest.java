package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.converter.ActualSpotConverter;
import java.time.Instant;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ActualSpotServiceImplTest {

    @Mock
    private ActualSpotConverter actualSpotConverter;
    @InjectMocks
    private ActualSpotServiceImpl actualSpotService;

    @Test
    void getAllAscending_givenProcessEventDirectories_shouldReturnActualSpots() {
        // given
        final String processEventDirectoriesJson = SSTUtils.getStringFromResource("/odata/process-event-directories.json");
        final List<ProcessEventDirectory> processEventDirectories =
                ODataUtils.readEntitySet(processEventDirectoriesJson, ProcessEventDirectory.class).getResults();
        final ActualSpot firstActualSpot = new ActualSpot();
        firstActualSpot.setActualBusinessTimestamp(getDateTimeString(Instant.MIN.getEpochSecond()));
        final ActualSpot secondActualSpot = new ActualSpot();
        secondActualSpot.setActualBusinessTimestamp(getDateTimeString(Instant.MAX.getEpochSecond()));

        when(actualSpotConverter.fromProcessEventDirectories(any())).thenReturn(asList(firstActualSpot, secondActualSpot));

        // when
        final List<ActualSpot> actualSpots = actualSpotService.getAllAscending(processEventDirectories);

        // then
        assertThat(actualSpots).isNotEmpty();
    }
}
