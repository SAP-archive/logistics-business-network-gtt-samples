package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.LOCATION_UPDATE;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static java.lang.String.format;
import static java.util.Collections.emptyList;
import static java.util.Comparator.comparing;
import static java.util.Comparator.comparingLong;
import static java.util.Comparator.nullsLast;
import static java.util.Objects.nonNull;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.FreightUnitHelper;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.FreightUnitService;
import com.sap.gtt.v2.sample.sst.odata.service.FreightUnitStopsForVpService;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.helper.RouteHelper;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
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
public class FreightUnitRouteService extends RouteAbstractService {

    private static final String FREIGHT_UNIT_ENDPOINT = "/FreightUnit";
    private static final String PLANNED_EVENTS_PARAM = "plannedEvents";
    private static final String SHIPMENT_TPS_PARAM = "shipmentTPs";
    private static final String SHIPMENT_PARAM = "shipment";
    private static final String STOPS_FOR_VP_PARAM = "stopsForVP";

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private FreightUnitStopsForVpService freightUnitStopsForVpService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private RouteHelper routeHelper;

    @Autowired
    private FreightUnitService freightUnitService;

    @Autowired
    private FreightUnitHelper freightUnitHelper;

    @Autowired
    private EventService eventService;

    @Override
    public Route getByTrackedProcessId(@NotNull final String freightUnitId) {
        final Route route = new Route();
        final List<PlannedEvent> plannedEvents = getPlannedEvents(freightUnitId);
        final List<StopsForVp> stopsForVp = freightUnitStopsForVpService.getAll(freightUnitId);
        final List<ActualSpot> actualSpots = getActualSpots(freightUnitId);
        final List<PlannedSpot> plannedSpots = plannedSpotConverter.fromPlannedEvents(plannedEvents);

        validateAndFillStopsForVp(route, stopsForVp);
        validateAndFillActualSpots(route, actualSpots);
        fillCurrentLocation(route);
        validateAndFillPlannedSpots(route, plannedSpots);

        routeHelper.updateRoutesConnection(route, plannedEvents);
        routeHelper.updateETAOfStopsForVp(route);
        routeHelper.updateEventStatusOfStopsForVp(route, plannedEvents);
        return route;
    }

    private List<ActualSpot> getActualSpots(final String freightUnitId) {
        final List<ProcessEventDirectory> actualEvents = getActualEvents(freightUnitId);
        final List<ProcessEventDirectory> actualLocationUpdateEvents = getActualLocationUpdateEvents(freightUnitId);
        actualEvents.addAll(actualLocationUpdateEvents);
        return actualSpotHelper.getAllAscending(actualEvents);
    }

    private List<ProcessEventDirectory> getActualEvents(final String freightUnitId) {
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByTrackedProcessId(freightUnitId);
        return ProcessEventDirectoryUtils.filterByWhitelistForRoutes(processEventDirectories);
    }

    private List<ProcessEventDirectory> getActualLocationUpdateEvents(final String freightUnitId) {
        final Optional<FreightUnit> freightUnitOpt = getFreightUnit(freightUnitId);
        return freightUnitOpt.map(this::retrieveLocationUpdateEvents).orElse(emptyList());
    }

    private List<ProcessEventDirectory> retrieveLocationUpdateEvents(final FreightUnit freightUnit) {
        final List<Shipment> shipments = freightUnitHelper.retrieveShipments(freightUnit);
        final List<PlannedEvent> freightUnitPlannedEvents = freightUnit.getPlannedEvents();
        final Map<Shipment, List<PlannedEvent>> freightUnitPlannedEventsByShipment =
                groupFreightUnitPlannedEventsByShipment(shipments, freightUnitPlannedEvents);
        return freightUnitPlannedEventsByShipment.entrySet().stream()
                .map(entrySet -> getLocationUpdateEventsFromShipment(entrySet.getKey(), entrySet.getValue()))
                .flatMap(List::stream)
                .collect(toList());
    }

    private Map<Shipment, List<PlannedEvent>> groupFreightUnitPlannedEventsByShipment(
            final List<Shipment> shipments, final List<PlannedEvent> freightUnitPlannedEvents) {
        return shipments.stream()
                .filter(Objects::nonNull)
                .filter(shipment -> nonNull(shipment.getTrackingId()))
                .collect(toMap(identity(), shipment -> findSuitablePlannedEventsByShipment(shipment, freightUnitPlannedEvents)));
    }

    private List<PlannedEvent> findSuitablePlannedEventsByShipment(
            final Shipment shipment, final List<PlannedEvent> freightUnitPlannedEvents) {
        return freightUnitPlannedEvents.stream()
                .filter(freightUnitPlannedEvent -> isSuitablePlannedEventForShipment(freightUnitPlannedEvent, shipment))
                .collect(toList());
    }

    private boolean isSuitablePlannedEventForShipment(final PlannedEvent freightUnitPlannedEvent, final Shipment shipment) {
        final String shipmentNo = shipment.getShipmentNo();
        final Optional<String> eventMatchKeyOpt = Optional.ofNullable(freightUnitPlannedEvent.getEventMatchKey());
        final String eventMatchKeySubString = eventMatchKeyOpt.map(eventMatchKey -> eventMatchKey.substring(0, 10)).orElse("-1");
        return Long.parseLong(shipmentNo) == Long.parseLong(eventMatchKeySubString);
    }

    private List<ProcessEventDirectory> getLocationUpdateEventsFromShipment(
            final Shipment shipment, final List<PlannedEvent> freightUnitPlannedEvents) {
        final Optional<String> minEventMatchKeyOpt = getMinEventMatchKey(freightUnitPlannedEvents);
        final Optional<String> maxEventMatchKeyOpt = getMaxEventMatchKey(freightUnitPlannedEvents);
        if (minEventMatchKeyOpt.isPresent() && maxEventMatchKeyOpt.isPresent()) {
            final String minEventMatchKey = minEventMatchKeyOpt.get();
            final String maxEventMatchKey = maxEventMatchKeyOpt.get();
            final List<StopsForVp> stopsForVp = findSuitableStopsForVp(shipment, minEventMatchKey, maxEventMatchKey);
            return getProcessEventDirectoriesWithLocationUpdateEvent(shipment, stopsForVp);
        }
        return emptyList();
    }

    private List<ProcessEventDirectory> getProcessEventDirectoriesWithLocationUpdateEvent(
            final Shipment shipment, final List<StopsForVp> stopsForVp) {
        final String shipmentId = shipment.getId().toString();
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByTrackedProcessId(shipmentId);
        final List<UUID> locationUpdateEventsIds = getLocationUpdateEventsIds(processEventDirectories);
        final List<Event> locationUpdateEvents = getLocationUpdateEvents(locationUpdateEventsIds);
        final List<Event> suitableLocationUpdateEvents = findSuitableLocationUpdateEvents(locationUpdateEvents, stopsForVp);
        return createProcessEventDirectories(suitableLocationUpdateEvents);
    }

    private List<Event> getLocationUpdateEvents(final List<UUID> locationUpdateEventsIds) {
        return locationUpdateEventsIds.isEmpty()
                ? emptyList()
                : eventService.getByEventType(LOCATION_UPDATE.getValue(), locationUpdateEventsIds);
    }

    private List<ProcessEventDirectory> createProcessEventDirectories(final List<Event> locationUpdateEvents) {
        return locationUpdateEvents.stream()
                .map(this::mapToProcessEventDirectory)
                .collect(toList());
    }

    private ProcessEventDirectory mapToProcessEventDirectory(final Event locationUpdateEvent) {
        final ProcessEventDirectory processEventDirectory = new ProcessEventDirectory();
        processEventDirectory.setEvent(locationUpdateEvent);
        return processEventDirectory;
    }

    private List<Event> findSuitableLocationUpdateEvents(
            final List<Event> locationUpdateEvents, final List<StopsForVp> stopsForVp) {
        return locationUpdateEvents.stream()
                .filter(event -> hasAnyEqualEventMatchKeys(event, stopsForVp))
                .collect(toList());
    }

    private boolean hasAnyEqualEventMatchKeys(final Event event, final List<StopsForVp> stopsForVp) {
        return stopsForVp.stream()
                .map(StopsForVp::getStopId)
                .filter(Objects::nonNull)
                .anyMatch(stopId -> stopId.equals(event.getRefPlannedEventMatchKey()));
    }

    private List<UUID> getLocationUpdateEventsIds(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(ped -> LOCATION_UPDATE.getValue().equals(getEventTypeShortName(ped.getEvent().getEventType())))
                .map(ProcessEventDirectory::getEventId)
                .collect(toList());
    }

    private Optional<String> getMinEventMatchKey(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .map(PlannedEvent::getEventMatchKey)
                .min(comparingLong(Long::parseLong));
    }

    private Optional<String> getMaxEventMatchKey(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .map(PlannedEvent::getEventMatchKey)
                .max(comparingLong(Long::parseLong));
    }

    private List<StopsForVp> findSuitableStopsForVp(
            final Shipment shipment, final String minEventMatchKey, final String maxEventMatchKey) {
        final List<StopsForVp> stopsForVp = shipment.getStopsForVp();
        return stopsForVp.stream()
                .filter(stopForVp -> isSuitableStopForVp(stopForVp, minEventMatchKey, maxEventMatchKey))
                .collect(toList());
    }

    private boolean isSuitableStopForVp(
            final StopsForVp stopForVp, final String minEventMatchKey, final String maxEventMatchKey) {
        final String stopId = stopForVp.getStopId();
        final long stopIdLong = Long.parseLong(stopId);
        final long minEventMatchKeyLong = Long.parseLong(minEventMatchKey);
        final long maxEventMatchKeyLong = Long.parseLong(maxEventMatchKey);
        return minEventMatchKeyLong < stopIdLong && stopIdLong <= maxEventMatchKeyLong;
    }

    private Optional<FreightUnit> getFreightUnit(final String freightUnitId) {
        final String uri = buildUriByFreightUnitId(freightUnitId);
        return freightUnitService.getByUri(uri);
    }

    private List<PlannedEvent> getPlannedEvents(final String trackedProcessId) {
        List<PlannedEvent> plannedEvents = plannedEventService.getAllByTrackedProcessId(trackedProcessId);
        return sortPlannedEvents(plannedEvents);
    }

    private List<PlannedEvent> sortPlannedEvents(final List<PlannedEvent> plannedEvents) {
        return plannedEvents.stream()
                .sorted(getPlannedEventComparator())
                .collect(toList());
    }

    private Comparator<? super PlannedEvent> getPlannedEventComparator() {
        return comparing(PlannedEvent::getPlannedBusinessTimestamp, nullsLast(Long::compareTo))
                .thenComparing(PlannedEvent::getPayloadSequence, nullsLast(Integer::compareTo));
    }

    private String buildUriByFreightUnitId(final String freightUnitId) {
        final String shipmentPlannedEventsParam = format("%s/%s/%s", SHIPMENT_TPS_PARAM, SHIPMENT_PARAM, PLANNED_EVENTS_PARAM);
        final String shipmentStopsForVpParam = format("%s/%s/%s", SHIPMENT_TPS_PARAM, SHIPMENT_PARAM, STOPS_FOR_VP_PARAM);
        final String expandParam = format("%s,%s,%s", PLANNED_EVENTS_PARAM, shipmentPlannedEventsParam, shipmentStopsForVpParam);
        return UriComponentsBuilder
                .fromPath(format("%s(guid'%s')", FREIGHT_UNIT_ENDPOINT, freightUnitId))
                .queryParam(EXPAND, expandParam)
                .build()
                .encode()
                .toUriString();
    }
}
