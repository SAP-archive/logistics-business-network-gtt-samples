package com.sap.gtt.v2.sample.sst.common.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.FILTER;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition.EDM_TYPE_GUID;
import static com.sap.gtt.v2.sample.sst.odata.filter.FilterExpressionBuilder.createFilterExpression;
import static java.lang.String.format;
import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;
import static org.apache.olingo.odata2.api.uri.expression.BinaryOperator.EQ;
import static org.apache.olingo.odata2.api.uri.expression.BinaryOperator.OR;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.model.CodeListValue;
import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.odata.filter.FilterCondition;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.UUID;
import javax.validation.constraints.NotNull;
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
public class EventServiceImpl implements EventService {

    private static final String ESTIMATED_ARRIVAL_PARAM = "estimatedArrival";
    private static final String LOCATION_TYPE_PARAM = "locationType";
    private static final String ID_PARAM = "id";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public Optional<Event> getById(@NotNull final String id, @NotNull final String eventType) {
        final String uri = buildUriById(id, eventType);
        final List<Event> events = gttCoreServiceClient.readEntitySetAll(uri, Event.class).getResults();
        return Optional.ofNullable(events.get(0));
    }

    @Override
    public List<Event> getByEventType(@NotNull final String eventType, @NotNull final List<UUID> ids) {
        final String uri = buildUriByIdsInList(eventType, ids);
        return gttCoreServiceClient.readEntitySetAll(uri, Event.class).getResults();
    }

    @Override
    public void create(@NotNull final String eventJson, @NotNull final String eventType) {
        final String uri = buildUri(eventType);
        gttCoreServiceClient.write(eventJson, uri);
    }

    @Override
    public String getEventTypesMetadata(@NotNull final String eventType) {
        return gttCoreServiceClient.getEventTypesMetadata(eventType);
    }

    @Override
    public List<CodeListValue> getCodeList(@NotNull final String codeListName, @NotNull final Locale locale) {
        return gttCoreServiceClient.getCodeList(codeListName, locale);
    }

    private String buildUriById(final String id, final String eventType) {
        return UriComponentsBuilder.fromPath(buildUri(eventType))
                .queryParam(FILTER, format("%s eq guid'%s'", ID_PARAM, id))
                .queryParam(EXPAND, ESTIMATED_ARRIVAL_PARAM)
                .build().encode().toUriString();
    }

    private String buildUriByIdsInList(final String eventType, final List<UUID> ids) {
        final String filterExpression = getFilterByIdsParam(ids).orElse(null);
        return UriComponentsBuilder.fromPath(buildUri(eventType))
                .queryParam(FILTER, filterExpression)
                .queryParam(EXPAND, LOCATION_TYPE_PARAM)
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

    private String buildUri(final String eventTypeName) {
        return UriComponentsBuilder.fromPath("/" + getEventTypeShortName(eventTypeName))
                .build().encode().toUriString();
    }
}
