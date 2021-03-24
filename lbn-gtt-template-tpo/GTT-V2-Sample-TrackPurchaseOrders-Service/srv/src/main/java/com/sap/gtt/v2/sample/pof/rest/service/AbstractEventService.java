package com.sap.gtt.v2.sample.pof.rest.service;

import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.Constants.DELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.constant.Constants.UNDELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;

@Component
public abstract class AbstractEventService {
    protected static final String GOODS_RECEIPT_URI = "/GoodsReceipt?$expand=eventProcesses";
    protected static final String PLANNED_EVENT_URI = "/PlannedEvent";
    protected static final String PROCESS_EVENT_DIRECTORY_URL_TEMPLATE = "/ProcessEventDirectory?$expand=event,plannedEvent&$filter=process_id eq guid'%s'";
    protected static final String GOODS_RECEIPT_URL_TEMPLATE = "/GoodsReceipt?$filter=eventProcesses/process_id eq guid'%s'";
    protected static final String CONFIRMATION_EVENT_URL_TEMPLATE = "/ConfirmationEvent?$filter=eventProcesses/process_id eq guid'%s'";
    protected static final String PLANNED_EVENTS_URL_TEMPLATE = "/PlannedEvent?$filter=process_id eq guid'%s'";

    protected static final String EVENT_PROCESS_ID_FILTER_PART = " eventProcesses/process_id eq guid'%s' ";
    protected static final String PROCESS_ID_FILTER_PART = " process_id eq guid'%s' ";
    protected static final String OR_EXPR = " or ";

    @Autowired
    protected GTTCoreServiceClient gttCoreServiceClient;

    protected <T> List<T> queryAllEvents(String uriTemplate, UUID id, Class<T> clazz) {
        String[] split = uriTemplate.split("\\?");
        String httpUrl = split[0];
        UriComponentsBuilder uriComponentsBuilder = UriComponentsBuilder
                .fromUriString(httpUrl);
        if (split.length > 1) {
            String query = String.format(split[1], id);
            uriComponentsBuilder.query(query);
        }
        String url = uriComponentsBuilder.build()
                .encode().toUriString();
        return queryAll(url, clazz);
    }

    protected <T> List<T> queryAllEvents(String uriTemplate, String filterPart, String delimiter, List<Object> args, Class<T> clazz) {
        String[] split = uriTemplate.split("\\?");
        String httpUrl = split[0];
        String initialQuery = split.length > 1 ? split[1] + "&" : EMPTY;
        List<String> filters = POFUtils.generateSplitLargeFilterExpr(filterPart, delimiter, args);
        return filters.parallelStream()
                .map(it -> initialQuery + FILTER + "=(" + it + ")")
                .map(it -> UriComponentsBuilder.fromUriString(httpUrl).query(it).encode().toUriString())
                .flatMap(it -> queryAll(it, clazz).stream())
                .collect(Collectors.toList());
    }

    protected <T> List<T> queryAll(String url, Class<T> clazz) {
        return gttCoreServiceClient.readEntitySetAll(url, clazz).getResults();
    }

    protected boolean isDeletionLatestEvent(UUID processId) {
        List<ProcessEventDirectory> eventDirectories = queryAllEvents(PROCESS_EVENT_DIRECTORY_URL_TEMPLATE, processId, ProcessEventDirectory.class);
        Optional<Event> max = eventDirectories.stream()
                .map(ProcessEventDirectory::getEvent)
                .filter(Objects::nonNull)
                .filter(event -> nonNull(event.getEventType())
                        && (POFUtils.isEventTypesEqual(UNDELETION_EVENT_ENTITY_NAME, event.getEventType())
                        || POFUtils.isEventTypesEqual(DELETION_EVENT_ENTITY_NAME, event.getEventType())))
                .max(Comparator.comparingLong(Event::getActualBusinessTimestamp));
        return max.isPresent() && max.get().getEventType().endsWith(DELETION_EVENT_ENTITY_NAME);
    }
}
