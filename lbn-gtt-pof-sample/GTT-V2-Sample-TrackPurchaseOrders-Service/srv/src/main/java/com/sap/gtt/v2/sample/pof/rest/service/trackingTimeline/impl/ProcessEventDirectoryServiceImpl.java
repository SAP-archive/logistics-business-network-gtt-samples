package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl;

import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import javax.validation.constraints.NotNull;
import java.util.List;

import static com.sap.gtt.v2.sample.pof.constant.Constants.*;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static java.lang.String.format;
import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;

@Service
public class ProcessEventDirectoryServiceImpl implements ProcessEventDirectoryService {
    public static final String PLANNED_EVENT_EXPAND = "plannedEvent";
    public static final String PROCESS_EVENT_DIRECTORY = "/ProcessEventDirectory";
    private GTTCoreServiceClient gttCoreServiceClient;

    public ProcessEventDirectoryServiceImpl(GTTCoreServiceClient gttCoreServiceClient) {
        this.gttCoreServiceClient = gttCoreServiceClient;
    }

    @Override
    public List<ProcessEventDirectory> getAllByDeliveryItemId(@NotNull String deliveryItemId) {
        String url = buildUriWithDeliveryItemId(deliveryItemId);
        ODataResultList<ProcessEventDirectory> oDataResultList = gttCoreServiceClient.readEntitySetAll(url,ProcessEventDirectory.class);
        return oDataResultList.getResults();
    }

    private String buildUriWithDeliveryItemId(final String deliveryItemId) {
        return UriComponentsBuilder.fromPath(PROCESS_EVENT_DIRECTORY)
                .queryParam(FILTER, format("%s eq guid'%s'", PROCESS_ID, deliveryItemId))
                .queryParam(EXPAND, format("%s,%s", PLANNED_EVENT_EXPAND, EVENT_EXPAND))
                .build().encode().toUriString();
    }

    @Override
    public List<ProcessEventDirectory> getWithoutPlannedEvent(
            @NotNull final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(processEventDirectory -> isNull(processEventDirectory.getPlannedEventId()))
                .collect(toList());
    }

}
