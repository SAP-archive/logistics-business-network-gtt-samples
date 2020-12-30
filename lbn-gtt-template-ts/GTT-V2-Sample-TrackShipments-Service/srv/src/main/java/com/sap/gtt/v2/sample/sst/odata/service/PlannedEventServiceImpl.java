package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.FILTER;
import static java.lang.String.format;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class PlannedEventServiceImpl implements PlannedEventService {

    private static final String PLANNED_EVENT_ENDPOINT = "/PlannedEvent";
    private static final String PROCESS_ID_PARAM = "process_id";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public List<PlannedEvent> getAllByShipmentId(@NotNull final String shipmentId) {
        final String uri = buildUriWithShipmentId(shipmentId);
        return gttCoreServiceClient.readEntitySetAll(uri, PlannedEvent.class).getResults();
    }

    private String buildUriWithShipmentId(final String shipmentId) {
        return UriComponentsBuilder.fromPath(PLANNED_EVENT_ENDPOINT)
                .queryParam(FILTER, format("%s eq guid'%s'", PROCESS_ID_PARAM, shipmentId))
                .build().encode().toUriString();
    }
}
