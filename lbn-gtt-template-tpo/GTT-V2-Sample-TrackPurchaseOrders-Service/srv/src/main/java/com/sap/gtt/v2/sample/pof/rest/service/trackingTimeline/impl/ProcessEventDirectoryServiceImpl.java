package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EVENT_EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.Constants.PROCESS_ID;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static java.lang.String.format;
import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

@Service
public class ProcessEventDirectoryServiceImpl implements ProcessEventDirectoryService {
    public static final String PLANNED_EVENT_EXPAND = "plannedEvent";
    public static final String PROCESS_EVENT_DIRECTORY = "/ProcessEventDirectory";
    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

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
