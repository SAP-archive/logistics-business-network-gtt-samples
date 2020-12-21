package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class PlannedEventServiceImplTest {

    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private PlannedEventServiceImpl plannedEventService;

    @Test
    void getAllByShipmentId_givenShipmentId_shouldReturnPlannedEvents() {
        // given
        final String shipmentId = randomUUID().toString();
        final String plannedEventsJson = getStringFromResource("/odata/planned-events.json");
        final ODataResultList<PlannedEvent> plannedEventODataResultList =
                ODataUtils.readEntitySet(plannedEventsJson, PlannedEvent.class);

        when(gttCoreServiceClient.readEntitySetAll(contains("/PlannedEvent"), eq(PlannedEvent.class)))
                .thenReturn(plannedEventODataResultList);

        // when
        final List<PlannedEvent> result = plannedEventService.getAllByShipmentId(shipmentId);

        // then
        assertThat(result).isNotEmpty();
    }
}
