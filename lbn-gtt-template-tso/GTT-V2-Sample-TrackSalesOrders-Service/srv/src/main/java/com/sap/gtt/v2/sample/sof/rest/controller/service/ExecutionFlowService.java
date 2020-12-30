package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.*;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.ExecutionFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.Lane;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.Node;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.CurrentLocation;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.Route;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.sap.gtt.v2.sample.sof.constant.Constants.*;
import static com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient.EXPAND;
import static com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient.FILTER;
import static java.lang.String.format;

@Service
public class ExecutionFlowService {

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Autowired
    private SOFService sofService;

    @Autowired
    private MapService mapService;

    public ExecutionFlow generateExecutionFlow(UUID deliveryItemId) {
        List<ProcessEventDirectory> processEventDirectories = getPED4TP(deliveryItemId);
        List<PlannedEvent> plannedEvents = sofService.getPlannedEvents4TP(deliveryItemId);
        // Estimated time of arrival data is retrieved from MapService
        Map<String, Long> etaMap = createETAMap(deliveryItemId);

        Map<PlannedEvent, List<EventEx>> plnAndActEventMap = createPlnAndActEventMap(plannedEvents, processEventDirectories);
        List<Node> nodes = generateNodes(plnAndActEventMap, etaMap);
        sortNodes(nodes);
        updateNodes(nodes);
        List<Lane> lanes = generateLanes(nodes);

        ExecutionFlow flow = new ExecutionFlow();
        flow.setLanes(lanes);
        flow.setNodes(nodes);
        return flow;
    }

    public List<EventEx> getEventReportHistory(UUID eventId) {
        List<EventEx> eventHistory = new ArrayList<>();
        String query = UriComponentsBuilder.fromUriString(URL_SPLITTER + PROCESS_EVENT_DIRECTORY_ENTITY_NAME)
                .queryParam(FILTER, format("plannedEvent_id eq guid'%s' or event_id eq guid'%s'", eventId, eventId))
                .queryParam(EXPAND, format("%s,%s", PLANNED_EVENT_EXPAND, EVENT_EXPAND))
                .build().encode().toUriString();

        ODataResultList<ProcessEventDirectory> response = gttCoreServiceClient.readEntitySetAll(query, ProcessEventDirectory.class);
        List<ProcessEventDirectory> processEventDirectories = response.getResults();

        if (processEventDirectories.stream().anyMatch(ped -> ped.getPlannedEventId() != null && eventId.compareTo(ped.getPlannedEventId()) == 0)) {
            processEventDirectories.stream()
                    .filter(ped -> ped.getEvent().getEventType().equals(ped.getPlannedEvent().getEventType()))
                    .forEach(ped -> {
                        EventEx eventEx = new EventEx();
                        BeanUtils.copyProperties(ped.getEvent(), eventEx);
                        eventHistory.add(eventEx);
                    });
        } else if (processEventDirectories.stream().anyMatch(ped -> eventId.compareTo(ped.getEventId()) == 0)) {
            Optional<ProcessEventDirectory> opt = processEventDirectories.stream()
                    .filter(ped -> eventId.compareTo(ped.getEventId()) == 0).findFirst();
            if (opt.isPresent()) {
                EventEx eventEx = new EventEx();
                BeanUtils.copyProperties(opt.get().getEvent(), eventEx);
                eventHistory.add(eventEx);
            }
        }
        eventHistory.sort(Comparator.comparing(EventEx::getActualBusinessTimestamp).reversed());
        eventHistory.forEach(this::updateEvent);
        return eventHistory;
    }

    private List<ProcessEventDirectory> getPED4TP(UUID tpId) {
        String query = UriComponentsBuilder.fromUriString(URL_SPLITTER + PROCESS_EVENT_DIRECTORY_ENTITY_NAME)
                .queryParam(FILTER, format("process_id eq guid'%s'", tpId))
                .queryParam(EXPAND, format("%s,%s", PLANNED_EVENT_EXPAND, EVENT_EXPAND))
                .build().encode().toUriString();
        ODataResultList<ProcessEventDirectory> result = gttCoreServiceClient.readEntitySetAll(query, ProcessEventDirectory.class);
        return result.getResults();
    }

    private Map<PlannedEvent, List<EventEx>> createPlnAndActEventMap(List<PlannedEvent> plannedEvents, List<ProcessEventDirectory> processEventDirectories) {
        Map<PlannedEvent, List<EventEx>> map = new HashMap<>();
        plannedEvents.forEach(plannedEvent -> map.put(plannedEvent, new ArrayList<>()));
        processEventDirectories.forEach(ped -> {
            List<EventEx> actualEvents;
            EventEx eventEx = new EventEx();
            if (ped.getPlannedEventId() != null && map.containsKey(ped.getPlannedEvent())) {
                actualEvents = map.get(ped.getPlannedEvent());
                BeanUtils.copyProperties(ped.getEvent(), eventEx);
                actualEvents.add(eventEx);
            } else if (ped.getPlannedEventId() == null) {
                actualEvents = map.computeIfAbsent(null, k -> new ArrayList<>());
                BeanUtils.copyProperties(ped.getEvent(), eventEx);
                actualEvents.add(eventEx);
            }
        });
        return map;
    }

    private List<Node> generateNodes(Map<PlannedEvent, List<EventEx>> plnAndActEventMap, Map<String, Long> etaMap) {
        List<Node> nodes = new ArrayList<>();
        Map<String, Location> locationMap = createLocationMap(plnAndActEventMap);
        Map<UUID, EventEx> eventMap = createEventMap(plnAndActEventMap);

        plnAndActEventMap.forEach((plannedEvent, actualEvents) -> {
            if (plannedEvent == null) {
                actualEvents.stream().filter(actualEvent -> SOFUtils.isEventTypeInWhiteList(actualEvent.getEventType()))
                        .forEach(actualEvent -> {
                            Node node = new Node();
                            Long actualBusinessTimestamp = actualEvent.getActualBusinessTimestamp();
                            String locationAltKey = actualEvent.getLocationAltKey();
                            String locationTypeCode = eventMap.get(actualEvent.getId()).getLocationTypeCode();
                            node.setEventId(actualEvent.getId());
                            node.setEventType(actualEvent.getEventType());
                            node.setActualBusinessTimestamp(actualBusinessTimestamp);
                            node.setActualAt(Instant.ofEpochMilli(actualBusinessTimestamp).toString());
                            node.setLocationAltKey(locationAltKey);
                            node.setLocationTypeCode(locationTypeCode);
                            nodes.add(node);
                        });
            } else if (SOFUtils.isEventTypeInWhiteList(plannedEvent.getEventType())) {
                Node node = new Node();
                Long plannedBusinessTimestamp = plannedEvent.getPlannedBusinessTimestamp();
                node.setEventId(plannedEvent.getId());
                node.setEventType(plannedEvent.getEventType());
                node.setEventStatus(plannedEvent.getEventStatusCode());
                node.setPlannedBusinessTimestamp(plannedBusinessTimestamp);
                node.setPlannedAt(Optional.ofNullable(plannedBusinessTimestamp)
                        .map(timestamp -> Instant.ofEpochMilli(timestamp).toString()).orElse(null));
                node.setLocationAltKey(plannedEvent.getLocationAltKey());
                node.setLocationDescription(Optional.ofNullable(locationMap.get(plannedEvent.getLocationAltKey()))
                        .map(Location::getLocationDescription).orElse(null));
                node.setPayloadSequence(plannedEvent.getPayloadSequence());

                if (Constants.EVENT_STATUS_EARLY_REPORTED.equals(plannedEvent.getEventStatusCode())
                        || Constants.EVENT_STATUS_REPORTED.equals(plannedEvent.getEventStatusCode())
                        || Constants.EVENT_STATUS_LATE_REPORTED.equals(plannedEvent.getEventStatusCode())) {
                    Optional<EventEx> lastCorrelatedEvent = getLastCorrelatedEvent(plannedEvent, actualEvents);
                    lastCorrelatedEvent.ifPresent(actualEvent -> {
                        Long actualBusinessTimestamp = actualEvent.getActualBusinessTimestamp();
                        String locationTypeCode = eventMap.get(actualEvent.getId()).getLocationTypeCode();
                        node.setActualBusinessTimestamp(actualBusinessTimestamp);
                        node.setActualAt(Instant.ofEpochMilli(actualBusinessTimestamp).toString());
                        node.setLocationAltKey(actualEvent.getLocationAltKey());
                        node.setLocationTypeCode(locationTypeCode);
                    });
                } else if (Constants.EVENT_STATUS_DELAYED.equals(plannedEvent.getEventStatusCode())) {
                    Optional<EventEx> maxActualBusinessTimestampDelayedEvent = actualEvents.stream()
                            .filter(actualEvent -> actualEvent.getEventType().contains(SHIPMENT_DELAY))
                            .max(Comparator.comparing(EventEx::getActualBusinessTimestamp));
                    maxActualBusinessTimestampDelayedEvent.ifPresent(actualEvent ->
                            node.setEventReasonText(actualEvent.getEventReasonText()));
                }
                // Set ETA for planned event
                if (plannedEvent.getEventType().contains(Constants.SHIPMENT_ARRIVAL) && plannedEvent.getEventMatchKey() != null) {
                    node.setEta(Optional.ofNullable(etaMap.get(plannedEvent.getEventMatchKey()))
                            .map(e -> Instant.ofEpochMilli(e).toString())
                            .orElse(null));
                }

                nodes.add(node);
            }
        });
        return nodes;
    }

    private Optional<EventEx> getLastCorrelatedEvent(PlannedEvent plannedEvent, List<EventEx> actualEvents) {
        Optional<UUID> lastCorrelatedEventId = Optional.ofNullable(plannedEvent.getLastProcessEventDirectory())
                .map(ProcessEventDirectory::getEventId);
        if (lastCorrelatedEventId.isPresent()) {
            UUID id = lastCorrelatedEventId.get();
            return actualEvents.stream()
                    .filter(actualEvent -> actualEvent.getEventType().equals(plannedEvent.getEventType())
                            && actualEvent.getId().compareTo(id) == 0)
                    .findFirst();
        }
        return Optional.empty();
    }

    private Map<String, Location> createLocationMap(Map<PlannedEvent, List<EventEx>> plnAndActEventMap) {
        Map<String, Location> locationMap = new HashMap<>();
        Set<String> locationAltKeys = new HashSet<>();
        locationAltKeys.addAll(plnAndActEventMap.keySet().stream().filter(Objects::nonNull).map(PlannedEvent::getLocationAltKey).collect(Collectors.toSet()));
        locationAltKeys.addAll(plnAndActEventMap.values().stream().flatMap(Collection::stream).map(Event::getLocationAltKey).collect(Collectors.toSet()));
        locationAltKeys.remove(null);

        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        locations.forEach(location -> locationMap.put(location.getLocationAltKey(), location));

        return locationMap;
    }

    private Map<UUID, EventEx> createEventMap(Map<PlannedEvent, List<EventEx>> plnAndActEventMap) {
        Map<UUID, EventEx> eventMap = new HashMap<>();
        Map<String, List<EventEx>> eventTypeAndEventMap = plnAndActEventMap.values().stream()
                .flatMap(Collection::stream).collect(Collectors.groupingBy(EventEx::getEventType));

        eventTypeAndEventMap.entrySet().stream()
                .filter(map -> SOFUtils.isEventTypeInWhiteList(map.getKey()))
                .forEach(map -> {
                    String eventTypeFull = map.getKey();
                    String eventType = eventTypeFull.substring(eventTypeFull.lastIndexOf('.') + 1);
                    List<FilterCondition> conditions = new ArrayList<>();
                    List<EventEx> events = map.getValue();
                    events.forEach(event ->
                            conditions.add(new FilterCondition(Constants.ID, FilterCondition.EDM_TYPE_GUID, event.getId().toString(), BinaryOperator.EQ)));
                    FilterExpression filter = FilterExpressionBuilder.createFilterExpression(conditions, BinaryOperator.OR);
                    assert filter != null;
                    String query = UriComponentsBuilder.fromUriString(URL_SPLITTER + eventType)
                            .queryParam(FILTER, filter.getExpressionString())
                            .build().encode().toUriString();
                    events = gttCoreServiceClient.readEntitySetAll(query, EventEx.class).getResults();
                    events.forEach(event -> eventMap.put(event.getId(), event));
                });

        return eventMap;
    }

    private void sortNodes(List<Node> nodes) {
        Comparator<Node> comparator = Comparator.comparing(Node::getActualBusinessTimestamp, Comparator.nullsLast(Comparator.naturalOrder()))
                .thenComparing(Node::getPlannedBusinessTimestamp, Comparator.nullsLast(Comparator.naturalOrder()))
                .thenComparing(Node::getPayloadSequence, Comparator.nullsLast(Comparator.naturalOrder()));
        nodes.sort(comparator);
    }

    private void updateNodes(List<Node> nodes) {
        IntStream.range(0, nodes.size() - 1).forEach(index -> updateNodeChildren(nodes.get(index), nodes.get(index + 1)));
        IntStream.range(0, nodes.size()).forEach((index -> updateNodeEventType(nodes.get(index))));
    }

    private void updateNodeChildren(Node current, Node next) {
        UUID id = next.getEventId();
        current.setChildren(Collections.singletonList(id));
    }

    private void updateNodeEventType(Node node) {
        String eventTypeFull = node.getEventType();
        String eventType = eventTypeFull.substring(eventTypeFull.lastIndexOf('.') + 1);
        node.setEventType(eventType);
    }

    private List<Lane> generateLanes(List<Node> nodes) {
        List<Lane> lanes = new ArrayList<>();
        int index = 0;
        for (Node node : nodes) {
            Lane lane = generateLane(node);
            lane.setPosition(index++);
            lanes.add(lane);
        }
        return lanes;
    }

    private Lane generateLane(Node node) {
        Lane lane = new Lane();
        lane.setEventId(node.getEventId());
        lane.setEventType(node.getEventType());
        lane.setLocationDescription(node.getLocationDescription());
        lane.setLocationTypeCode(node.getLocationTypeCode());
        return lane;
    }

    private void updateEvent(EventEx event) {
        // Update eventType (e.g. {namespace}.Shipment.CheckIn -> CheckIn)
        String eventTypeFull = event.getEventType();
        String eventType = eventTypeFull.substring(eventTypeFull.lastIndexOf('.') + 1);
        event.setEventType(eventType);

        // Update actualAt according to actualBusinessTimestamp
        Long actualBusinessTimestamp = event.getActualBusinessTimestamp();
        event.setActualAt(Instant.ofEpochMilli(actualBusinessTimestamp).toString());
    }

    private Map<String, Long> createETAMap(UUID deliveryItemId) {
        Map<String, Route> actualRoutes = mapService.getActualRoute(new HashSet<>(), new HashSet<>(), deliveryItemId.toString());
        mapService.getCurrentLocations(actualRoutes);
        List<EstimatedArrival> estimatedArrivals = actualRoutes.entrySet().stream()
                .filter(m -> m.getKey().contains(Constants.SHIPMENT))
                .map(Map.Entry::getValue)
                .map(Route::getCurrentLocation)
                .filter(Objects::nonNull)
                .map(CurrentLocation::getEstimatedArrival)
                .filter(Objects::nonNull)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());

        return estimatedArrivals.stream()
                .collect(Collectors.toMap(EstimatedArrival::getStopId, EstimatedArrival::getEstimatedArrivalTime));
    }

}