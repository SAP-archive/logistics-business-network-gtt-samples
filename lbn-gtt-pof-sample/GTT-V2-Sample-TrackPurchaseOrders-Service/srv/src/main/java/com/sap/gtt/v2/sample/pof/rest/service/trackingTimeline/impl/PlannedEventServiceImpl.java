package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl;

import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.PlannedEventService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import javax.validation.constraints.NotNull;
import java.util.List;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static java.lang.String.format;

@Service
public class PlannedEventServiceImpl implements PlannedEventService {
    private static final String PLANNED_EVENT_ENDPOINT = "/PlannedEvent";
    private static final String PROCESS_ID_PARAM = "process_id";
    public static final String LAST_PROCESS_EVENT_DIRECTORY_EVENT = "lastProcessEventDirectory/event";

    private final GTTCoreServiceClient gttCoreServiceClient;

    public PlannedEventServiceImpl(final GTTCoreServiceClient gttCoreServiceClient) {
        this.gttCoreServiceClient = gttCoreServiceClient;
    }
    @Override
    public List<PlannedEvent> getAllByDeliveryItemId(@NotNull final String deliveryItemId) {
        String url = buildUriWithDeliveryItemId(deliveryItemId);
        ODataResultList<PlannedEvent> resultList = gttCoreServiceClient.readEntitySetAll(url,PlannedEvent.class);
        return resultList.getResults();
    }

    private String buildUriWithDeliveryItemId(final String deliveryItemId) {
        return UriComponentsBuilder.fromPath(PLANNED_EVENT_ENDPOINT)
                .queryParam(FILTER, format("%s eq guid'%s'", PROCESS_ID_PARAM, deliveryItemId))
                .queryParam(EXPAND,format("%s", LAST_PROCESS_EVENT_DIRECTORY_EVENT))
                .build().encode().toUriString();
    }
}
