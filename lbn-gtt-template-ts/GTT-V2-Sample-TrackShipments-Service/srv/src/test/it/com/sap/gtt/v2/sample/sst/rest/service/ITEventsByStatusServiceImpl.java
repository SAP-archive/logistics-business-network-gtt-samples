package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.springframework.http.HttpMethod.GET;

import com.sap.gtt.v2.sample.sst.ITCommonAbstract;
import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;

class ITEventsByStatusServiceImpl extends ITCommonAbstract {

    @Autowired
    private EventsByStatusService eventsByStatusService;

    @Test
    void getByShipmentId() {
        // given
        final String shipmentId = randomUUID().toString();
        final String eventStatusesJson = getStringFromResource("/odata/event-statuses.json");
        final String plannedEventsJson = getStringFromResource("/odata/planned-events.json");

        doReturn(ResponseEntity.ok().body(plannedEventsJson))
                .when(restTemplate)
                .exchange(contains("/PlannedEvent"), eq(GET), any(HttpEntity.class), eq(String.class));
        doReturn(ResponseEntity.ok().body(eventStatusesJson))
                .when(restTemplate)
                .exchange(contains("/EventStatus"), eq(GET), any(HttpEntity.class), eq(String.class));

        // when
        final List<EventsByStatus> eventsByStatuses = eventsByStatusService.getByShipmentId(shipmentId);

        // then
        assertThat(eventsByStatuses)
                .hasSize(4)
                .extracting(EventsByStatus::getStatusCode, EventsByStatus::getCount)
                .containsExactlyInAnyOrder(
                        tuple("PLANNED", 4),
                        tuple("OVERDUE", 2),
                        tuple("DELAYED", 2),
                        tuple("REPORTED", 2)
                );
    }
}
