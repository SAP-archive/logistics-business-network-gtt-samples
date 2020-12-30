package com.sap.gtt.v2.sample.sst.common.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.FILTER;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_GUID;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterExpressionBuilder.createFilterExpression;
import static java.lang.String.format;
import static java.util.Collections.emptyList;
import static java.util.Objects.isNull;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static org.apache.olingo.odata2.api.uri.expression.BinaryOperator.EQ;
import static org.apache.olingo.odata2.api.uri.expression.BinaryOperator.OR;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import javax.validation.constraints.NotNull;
import one.util.streamex.StreamEx;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class ProcessEventDirectoryServiceImpl implements ProcessEventDirectoryService {

    private static final String PROCESS_EVENT_DIRECTORY_ENDPOINT = "/ProcessEventDirectory";
    private static final String PROCESS_ID_PARAM = "process_id";
    private static final String ID_PARAM = "id";
    private static final String PLANNED_EVENT_FIELD = "plannedEvent";
    private static final String EVENT_FIELD = "event";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public List<ProcessEventDirectory> getByShipmentId(@NotNull final String shipmentId) {
        final String uri = buildUriByShipmentId(shipmentId);
        return gttCoreServiceClient.readEntitySetAll(uri, ProcessEventDirectory.class).getResults();
    }

    @Override
    public List<ProcessEventDirectory> getByIds(@NotNull final List<UUID> ids) {
        final String uri = buildUriByIdsInList(ids);
        return gttCoreServiceClient.readEntitySetAll(uri, ProcessEventDirectory.class).getResults();
    }

    @Override
    public Map<ProcessEventDirectory, List<ProcessEventDirectory>> getActualEventsByPlannedEvents(
            @NotNull final List<ProcessEventDirectory> events) {
        final List<ProcessEventDirectory> uniquePlannedEvents = getUniquePlannedEvents(events);
        final List<ProcessEventDirectory> correlatedProcessEventDirectories =
                retrieveCorrelatedProcessEventDirectories(uniquePlannedEvents);
        return correlatedProcessEventDirectories.stream()
                .collect(toMap(Function.identity(), it -> getActualEventsByPlannedEvent(it, events)));
    }

    @Override
    public List<ProcessEventDirectory> getWithoutPlannedEvent(
            @NotNull final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(processEventDirectory -> isNull(processEventDirectory.getPlannedEventId()))
                .collect(toList());
    }

    private List<ProcessEventDirectory> getActualEventsByPlannedEvent(
            final ProcessEventDirectory plannedEvent, final List<ProcessEventDirectory> events) {
        final UUID plannedEventId = plannedEvent.getPlannedEventId();
        return events.stream()
                .filter(event -> plannedEventId.equals(event.getPlannedEventId()))
                .collect(toList());
    }

    private List<ProcessEventDirectory> getUniquePlannedEvents(final List<ProcessEventDirectory> events) {
        return StreamEx.of(events)
                .filter(processEventDirectory -> nonNull(processEventDirectory.getPlannedEventId()))
                .distinct(ProcessEventDirectory::getPlannedEventId)
                .collect(toList());
    }

    private List<ProcessEventDirectory> retrieveCorrelatedProcessEventDirectories(
            final List<ProcessEventDirectory> processEventDirectories) {
        final List<UUID> correlatedProcessEventDirectoriesIds = getLastProcessEventDirectoriesIds(processEventDirectories);
        return correlatedProcessEventDirectoriesIds.isEmpty()
                ? emptyList()
                : getByIds(correlatedProcessEventDirectoriesIds);
    }

    private List<UUID> getLastProcessEventDirectoriesIds(final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .map(ProcessEventDirectory::getPlannedEvent)
                .filter(Objects::nonNull)
                .map(PlannedEvent::getLastProcessEventDirectoryId)
                .filter(Objects::nonNull)
                .collect(toList());
    }

    private String buildUriByIdsInList(final List<UUID> ids) {
        final String filterExpression = getFilterByIdsParam(ids).orElse(null);
        return UriComponentsBuilder.fromPath(PROCESS_EVENT_DIRECTORY_ENDPOINT)
                .queryParam(FILTER, filterExpression)
                .queryParam(EXPAND, format("%s,%s", PLANNED_EVENT_FIELD, EVENT_FIELD))
                .build().encode().toUriString();
    }

    private Optional<String> getFilterByIdsParam(final List<UUID> ids) {
        final List<FilterCondition> filterConditions = getFilterConditionsByIds(ids);
        final FilterExpression filterExpression = createFilterExpression(filterConditions, OR);
        return isNull(filterExpression)
                ? Optional.empty()
                : Optional.of(filterExpression.getExpressionString());
    }

    private List<FilterCondition> getFilterConditionsByIds(final List<UUID> ids) {
        return ids.stream()
                .map(UUID::toString)
                .map(id -> new FilterCondition(ID_PARAM, EDM_TYPE_GUID, id, EQ))
                .collect(toList());
    }

    private String buildUriByShipmentId(final String shipmentId) {
        return UriComponentsBuilder.fromPath(PROCESS_EVENT_DIRECTORY_ENDPOINT)
                .queryParam(FILTER, format("%s eq guid'%s'", PROCESS_ID_PARAM, shipmentId))
                .queryParam(EXPAND, format("%s,%s", PLANNED_EVENT_FIELD, EVENT_FIELD))
                .build().encode().toUriString();
    }
}
