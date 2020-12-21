package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.ALL_PAGES;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.INLINECOUNT;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.model.EventStatus;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class EventStatusServiceImpl implements EventStatusService {

    private static final String EVENT_STATUS_ENDPOINT = "/EventStatus";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public List<EventStatus> getAll() {
        final String uri = buildUri();
        return gttCoreServiceClient.readEntitySetAll(uri, EventStatus.class).getResults();
    }

    private String buildUri() {
        return UriComponentsBuilder.fromPath(EVENT_STATUS_ENDPOINT)
                .queryParam(INLINECOUNT, ALL_PAGES)
                .build().encode().toUriString();
    }
}
