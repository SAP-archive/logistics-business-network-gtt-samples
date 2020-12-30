package com.sap.gtt.v2.sample.pof.service;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.*;
import com.sap.gtt.v2.sample.pof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.pof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.Shipment;
import com.sap.gtt.v2.sample.pof.odata.model.StopsForVP;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static java.lang.String.format;
import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;

@Service
public class MapService {

    private static final Logger logger = LoggerFactory.getLogger(MapService.class);

    public static final String ACTUAL_SPOT = "ActualSpot";
    public static final String PLANNED_LOCATION = "PlannedLocation";
    private static final String REGEX_FIRST_REPLACEMENT = "$1";
    private static final String REGEX_GET_SECOND = ".+:([^:].+):[^:].+$";
    private static final String REGEX_GET_FIRST_PART = "(.+):[^:].+:[^:].+$";
    private static final String REGEX_GET_LAST_PART = ".+:([^:].+)$";
    private static final int SHIPMENT_ID_LENGTH = 10;
    public static final String SHIPMENT = "Shipment";
    public static final String STOPS_FOR_VP = "stopsForVP";
    public static final String ALT_KEY = "altKey";

    private final GTTCoreServiceClient gttCoreServiceClient;
    private final POFService pofService;

    public MapService(GTTCoreServiceClient gttCoreServiceClient, POFService pofService) {
        this.gttCoreServiceClient = gttCoreServiceClient;
        this.pofService = pofService;
    }

    public LocationDTO getLocationDetail(Location location) {
        if (location == null) {
            return null;
        }
        LocationDTO locationDTO = new LocationDTO();
        locationDTO.setFormattedAddress(location.getFormattedAddress());
        locationDTO.setExternalId(location.getExternalId());
        locationDTO.setLocationAltKey(location.getLocationAltKey());
        locationDTO.setLocationId(location.getLocationId());
        locationDTO.setLocationDescription(location.getLocationDescription());
        locationDTO.setObjectTypeDescription(location.getObjectTypeDescription());
        locationDTO.setSourceSystem(location.getSourceSystem());
        locationDTO.setLatitude(location.getLatitude());
        locationDTO.setLongitude(location.getLongitude());
        locationDTO.setObjectTypeCode(location.getObjectTypeCode());
        locationDTO.setLocationTypeDescription(location.getObjectTypeDescription());
        return locationDTO;
    }


    /**
     *
     * @param deliveryItemId
     * @return routes list of the delivery item.
     * get all the actual routes of the delivery item.
     * set location in actual spots with the locationAltKey.
     * get currentLocations, that is to say, get the last actual spot as the currentLocation.
     * get next stops, that is to say, get the ref planned event of the current location.
     * get all planned routes of the delivery item.(key: groupId)
     * merge the planned routes and actual routes. get the result.
     * set transportationMode and stops in routes.(transportation mode and stops only will be shown in shipment)
     * set eta.
     * set destination of the route.(generate the locationAltKey field from delivery item and then get location from location master)
     * remove the reported planned event in the route with the event status white list.
     * fill coordinates of the actual spots with the actual spot'location master. if the actual spots coordinates is not valid and its event match key is not blank
     * remove the invalid coordinates including the actual spot and planned spot. and as the same time, add the invalid or missing information in the route.
     * set location type in the actual spot.(get the location type from user defined event )
     *
     */
    public List<Route> getRoutes(String deliveryItemId) {
        Set<String> trackingIdTypes = new HashSet<>();
        Set<String> partyIds = new HashSet<>();
        Map<String, Route> actualRoutes = getActualRoute(trackingIdTypes, partyIds, deliveryItemId);
        setLocationInActualSpot(actualRoutes);
        getCurrentLocations(actualRoutes);
        getNextStops(actualRoutes, deliveryItemId);
        Map<String, Route> plannedRoutes = getPlannedRoutes(deliveryItemId);
        List<Route> result = mergeRoutes(trackingIdTypes, partyIds, actualRoutes, plannedRoutes);

        getTransportationModeAndStops(result);
        setETA(result);
        setDestination(result, deliveryItemId);
        fillCoordinates(result);
        removeInvalidCoordinates(result);
        setLocationType4ActualSpot(result);
        return result;
    }

    public void setLocationType4ActualSpot(List<Route> result) {
        Map<UUID, EventEx> eventExMap = createEventExMap(result);
        result.forEach(route -> route.getActualSpots().forEach(actualSpot -> actualSpot.setLocationTypeCode(eventExMap.getOrDefault(actualSpot.getEventId(), new EventEx()).getLocationTypeCode())));

    }

    private Map<UUID, EventEx> createEventExMap(List<Route> result) {
        Map<String, Set<UUID>> eventTypeMaps = new HashMap<>();
        createEventTypeMap(result, eventTypeMaps);
        Map<UUID, EventEx> eventExMap = new HashMap<>();
        eventTypeMaps.forEach((k, v) -> {
            List<FilterCondition> filterConditions = new LinkedList<>();
            v.forEach(eventId ->
                    filterConditions.add(new FilterCondition(Constants.ID, FilterCondition.EDM_TYPE_GUID, eventId.toString(), BinaryOperator.EQ))
            );
            FilterExpression filter = FilterExpressionBuilder.createFilterExpression(filterConditions, BinaryOperator.OR);
            if (filter != null) {
                String url = UriComponentsBuilder.fromUriString(Constants.URL_SPLITTER+k)
                        .queryParam(FILTER,filter.getExpressionString())
                        .build().encode().toUriString();
                List<EventEx> events = gttCoreServiceClient.readEntitySetAll(url, EventEx.class).getResults();
                events.forEach(eventEx -> eventExMap.put(eventEx.getId(), eventEx));
            }
        });
        return eventExMap;
    }

    public void createEventTypeMap(List<Route> result, Map<String, Set<UUID>> eventTypeMaps) {
        if (CollectionUtils.isEmpty(result)) {
            return;
        }
        result.stream()
                .filter(route -> !CollectionUtils.isEmpty(route.getActualSpots()))
                .forEach(route -> createEventTypeMap(eventTypeMaps, route));
    }

    public void createEventTypeMap(Map<String, Set<UUID>> eventTypeMaps, Route route) {
        route.getActualSpots().forEach(actualSpot -> {
            if (POFUtils.isEventTypeInWhiteList(actualSpot.getEventType())) {
                if (eventTypeMaps.containsKey(actualSpot.getEventType())) {
                    eventTypeMaps.get(actualSpot.getEventType()).add(actualSpot.getEventId());
                } else {
                    Set<UUID> set = new HashSet<>();
                    set.add(actualSpot.getEventId());
                    eventTypeMaps.put(actualSpot.getEventType(), set);
                }
            }
        });
    }

    private void removeInvalidCoordinates(List<Route> result) {
        result.forEach(route -> {
            route.getActualSpots().removeIf(actualSpot -> !validLocation(actualSpot.getLongitude(), actualSpot.getLatitude(), route, PLANNED_LOCATION));
            route.getPlannedSpots().removeIf(plannedSpot -> !validLocation(plannedSpot.getLongitude(), plannedSpot.getLatitude(), route, PLANNED_LOCATION));
        });
    }


    private void setDestination(List<Route> result, String deliveryItemId) {
        String url = UriComponentsBuilder.fromUriString(format("/InboundDeliveryItem(guid'%s')",deliveryItemId)).build().encode().toUriString();
        InboundDeliveryItem deliveryItem = gttCoreServiceClient.readEntity(url, InboundDeliveryItem.class);
        String destinationAltKey = POFUtils.generateLocationAltKey(deliveryItem.getPartyId(), deliveryItem.getLogicalSystem(),
                deliveryItem.getPlantLocationType().getCode(), deliveryItem.getDestination());
        Location location = gttCoreServiceClient.getLocation(destinationAltKey);
        LocationDTO locationDTO = getLocationDetail(location);
        result.forEach(route -> {
            route.setDestinationLocation(locationDTO);
            route.setDestinationLocationAltKey(destinationAltKey);
        });
    }

    private void fillCoordinates(List<Route> result) {
        result.forEach(route -> {
                    route.getActualSpots().forEach(actualSpot -> {
                        if (!validLocation(actualSpot.getLongitude(), actualSpot.getLatitude(), route, ACTUAL_SPOT) && (StringUtils.isNotBlank(actualSpot.getEventMatchKey()))) {
                            LocationDTO locationDTO = actualSpot.getLocation();
                            if (locationDTO != null) {
                                actualSpot.setLongitude(locationDTO.getLongitude());
                                actualSpot.setLatitude(locationDTO.getLatitude());
                            }
                        }
                    });
                    route.getActualSpots().removeIf(actualSpot -> !validLocation(actualSpot.getLongitude(), actualSpot.getLatitude(), route, ACTUAL_SPOT) && StringUtils.isBlank(actualSpot.getEventMatchKey()));
                }
        );

    }

    /**
     *
     * set location in actual spots with the locationAltKey.
     *
     */
    private void setLocationInActualSpot(Map<String, Route> actualRoutes) {
        List<Route> routes = new ArrayList<>(actualRoutes.values());
        Set<String> locationAltKeys = new HashSet<>();
        routes.forEach(route -> locationAltKeys.addAll(route.getLocationAltKey()));
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, Location> locationMap = createLocationMap(locations);
        routes.forEach(route -> route.getActualSpots()
                .forEach(actualSpot -> {
            Location location = locationMap.get(actualSpot.getLocationAltKey());
            LocationDTO locationDTO = getLocationDetail(location);
            actualSpot.setLocation(locationDTO);
            if (locationDTO != null ) {
                actualSpot.setLocationDescription(locationDTO.getLocationDescription());
                actualSpot.setObjectTypeCode(locationDTO.getObjectTypeCode());
            }
        }));
    }

    /**
     * @expectResult: eta need to be set in the route
     * @param: the entire routes
     * get the etas from currentLocation of route.
     * set the eta to planned spot with two condition:
     * condition 1: event type of the planned event should be shipment.arrival
     * condition 2: the planned spot's event match key(groupId) needs to be same as the eta's stop.
     */
    private void setETA(List<Route> result) {
        if (CollectionUtils.isEmpty(result)) {
            return;
        }
        for (Route route : result) {
            if (CollectionUtils.isEmpty(route.getPlannedSpots()) || route.getCurrentLocation() == null) {
                continue;
            }
            List<EstimatedArrival> estimatedArrivals = route.getCurrentLocation().getEstimatedArrival();
            if (CollectionUtils.isEmpty(estimatedArrivals)) {
                continue;
            }
            Map<String, EstimatedArrival> estimatedArrivalMap = createEstimatedArrivalMap(estimatedArrivals);
            route.getPlannedSpots().forEach(plannedSpot -> {
                if (plannedSpot.getEventTypeOrign().contains(Constants.SHIPMENT_ARRIVAL)) {
                    plannedSpot.setEstimatedArrival(estimatedArrivalMap.get(plannedSpot.getEventMatchKey()));
                }
            });
        }
    }

    private Map<String, EstimatedArrival> createEstimatedArrivalMap(List<EstimatedArrival> estimatedArrivals) {
        Map<String, EstimatedArrival> estimatedArrivalHashMap = new HashMap<>();
        estimatedArrivals.forEach(estimatedArrival -> estimatedArrivalHashMap.put(estimatedArrival.getStopId(), estimatedArrival));
        return estimatedArrivalHashMap;
    }

    private void getTransportationModeAndStops(List<Route> result) {
        if (CollectionUtils.isEmpty(result)) {
            return;
        }
        for (Route route : result) {
            String altKey = route.getAltKey();
            if (StringUtils.contains(altKey, Constants.SHIPMENT)) {
                Shipment shipment = getShipment(altKey);
                route.setTransportationModeCode(shipment.getTransportationModeCode());
                route.setExecutionStatusCode(shipment.getExecutionStatusCode());
                setRouteStops(route, shipment);
            }
            if (!CollectionUtils.isEmpty(route.getPlannedSpots())) {
                route.setFirstPlannedEventId(route.getPlannedSpots().get(0).getPlannedEventId());
            }
        }
    }

    private void setRouteStops(Route route, Shipment shipment) {
        try {
            route.setStopsForVPS(shipment.getStopsForVP());
            Set<String> locationAltKeys = generateLocationAltKeyWithStopsForVP(shipment, shipment.getStopsForVP());
            List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
            Map<String, Location> locationMap = createLocationMap(locations);
            route.getStopsForVPS().forEach(stopsForVP -> {
                Location location = locationMap.get(stopsForVP.getLocationAltKey());
                stopsForVP.setLocation(getLocationDetail(location));
            });
        } catch (Exception e) {
            logger.error("Route stops calculation finished with error: ", e);
            route.setStopsForVPS(new ArrayList<>());
        }
    }

    private Set<String> generateLocationAltKeyWithStopsForVP(Shipment shipment, List<StopsForVP> stops) {
        stops.forEach(stop -> {
            String locationAltKey = POFUtils.generateLocationAltKey(shipment.getPartyId(), shipment.getLogicalSystem(), stop.getLocationTypeCode(), stop.getLocationId());
            stop.setLocationAltKey(locationAltKey);
        });
        return stops.stream().map(StopsForVP::getLocationAltKey).collect(Collectors.toSet());
    }

    private Shipment getShipment(String altKey) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        filterConditions.add(new FilterCondition(ALT_KEY, FilterCondition.EDM_TYPE_STRING, altKey, BinaryOperator.EQ));

        List<String> expand = new ArrayList<>();
        expand.add(STOPS_FOR_VP);

        String url = POFUtils.generateUrl(SHIPMENT, filterConditions, BinaryOperator.AND, expand, null);
        ODataResultList<Shipment> resultList = gttCoreServiceClient.readEntitySetAll(url, Shipment.class);
        if (CollectionUtils.isEmpty(resultList.getResults())) {
            return new Shipment();
        }
        return resultList.getResults().get(0);
    }


    public void getCurrentLocations(Map<String, Route> actualRoutes) {
        if (actualRoutes.isEmpty()) {
            return;
        }
        for (Route route : actualRoutes.values()) {
            getCurrentLocation(route);
        }
    }

    public void getCurrentLocation(Route actualRoute) {
        ActualSpot spot = actualRoute.getLastActualSpot();
        if (spot == null) {
            return;
        }
        UUID eventId = spot.getEventId();
        String eventType = spot.getEventTypeOrign();
        String url = this.generateCurrentLocationUrl(eventType, eventId);
        ODataResultList<EventEx> events = gttCoreServiceClient.readEntitySetAll(url, EventEx.class);
        if (events.getResults().isEmpty()) {
            return;
        }
        EventEx event = events.getResults().get(0);
        setCurrentLocation(actualRoute, event);
    }

    public void getNextStops(Map<String, Route> actualRoutes, String deliveryItemId) {
        if (actualRoutes.isEmpty()) {
            return;
        }
        for (Route route : actualRoutes.values()) {
            getNextStop(route, deliveryItemId);
        }
    }

    public void getNextStop(Route actualRoute, String deliveryItemId) {
        PlannedEvent plannedEvent = getRefPlannedEvent(actualRoute.getCurrentLocation(), deliveryItemId);
        if (plannedEvent != null) {
            LocationDTO locationDTO = getLocationDTO(plannedEvent.getLocationAltKey());
            plannedEvent.setLocationDTO(locationDTO);
        }
        actualRoute.setNextStopEvent(plannedEvent);
    }

    public void setCurrentLocation(Route route, EventEx event) {
        CurrentLocation currentLocation = new CurrentLocation();
        currentLocation.setAltKey(event.getAltKey());
        currentLocation.setRefPlannedEventMatchKey(event.getRefPlannedEventMatchKey());
        currentLocation.setEventId(event.getId());
        currentLocation.setEventTypeOrign(event.getEventType());
        currentLocation.setRefPlannedEventLocationAltKey(event.getRefPlannedEventLocationAltKey());
        currentLocation.setRefPlannedEventType(event.getRefPlannedEventType());
        currentLocation.setLatitude(event.getLatitude());
        currentLocation.setLongitude(event.getLongitude());
        currentLocation.setEstimatedArrival(event.getEstimatedArrival());
        route.setCurrentLocation(currentLocation);
    }

    private String generateCurrentLocationUrl(String eventType, UUID eventId) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        filterConditions.add(new FilterCondition("id", FilterCondition.EDM_TYPE_GUID, eventId.toString(), BinaryOperator.EQ));

        List<String> expand = null;
        if (POFUtils.isEventTypeWithEstimatedArrival(eventType)) {
            expand = new ArrayList<>();
            expand.add("estimatedArrival");
        }
        eventType = eventType.substring(eventType.lastIndexOf('.') + 1);
        String targetEntityName = eventType;
        return POFUtils.generateUrl(targetEntityName, filterConditions, BinaryOperator.AND, expand, null);
    }

    /**
     *
     * @param trackingIdTypes
     * @param partyIds
     * @param actualRoutes
     * @param plannedRoutes
     * @return the entire routes with planned route and actual route
     *
     */
    public List<Route> mergeRoutes(Set<String> trackingIdTypes, Set<String> partyIds, Map<String, Route> actualRoutes,
                                   Map<String, Route> plannedRoutes) {
        if (actualRoutes.isEmpty()) {
                return new ArrayList<>(plannedRoutes.values());
        }
        if (plannedRoutes.isEmpty()) {
            return new ArrayList<>(actualRoutes.values());
        }
        List<Route> plannedR = new LinkedList<>(plannedRoutes.values());
        plannedRoutes.forEach((k, plannedRoute) -> {
            for (String partyId : partyIds) {
                for (String trackingIdType : trackingIdTypes) {
                    groupEventsWithGroupId(actualRoutes, plannedR, k, plannedRoute, partyId, trackingIdType);
                }
            }

        });
        List<Route> routes = new ArrayList<>(actualRoutes.values());
        routes.addAll(plannedR);
        return routes;
    }

    /**
     *
     * @param actualRoutes
     * @param plannedR
     * @param k   groupId of the planned route
     * @param plannedRoute
     * @param partyId
     * @param trackingIdType
     * group events with groupId
     * two group condition:
     * condition 1: the altKey in actual evnet contains the groupId of the planned event.
     * condition 2.1 : the next planned event of the actual event group is in the planned event groups.
     * condition 2.2 : the planned event of the current location's event reported on in the actual event group is in the planned event groups.
     *
     */
    public void groupEventsWithGroupId(Map<String, Route> actualRoutes, List<Route> plannedR, String k, Route plannedRoute, String partyId, String trackingIdType) {
        String altKey = partyId.concat(":").concat(trackingIdType).concat(":").concat(k);
        if (actualRoutes.containsKey(altKey)) {
            Route route = actualRoutes.get(altKey);
            route.addPlannedSpots(plannedRoute.getPlannedSpots());
            actualRoutes.put(altKey, route);
            plannedR.remove(plannedRoute);
        }
    }

    public PlannedEvent getRefPlannedEvent(CurrentLocation currentLocation, String deliveryItemId) {
        if (currentLocation == null) {
            return null;
        }
        String eventType = currentLocation.getRefPlannedEventType();
        String eventMatchKey = currentLocation.getRefPlannedEventMatchKey();
        if (StringUtils.isAllBlank(eventMatchKey, eventType)) {
            return null;
        }
        String url = generatePlannedEventUrl(eventMatchKey, eventType, deliveryItemId);
        ODataResultList<PlannedEvent> entitySet = gttCoreServiceClient.readEntitySetAll(url, PlannedEvent.class);
        return entitySet.getResults().isEmpty() ? null : entitySet.getResults().get(0);
    }

    private String generatePlannedEventUrl(String eventMatchKey, String eventType, String deliveryItemId) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        filterConditions.add(new FilterCondition("eventMatchKey", FilterCondition.EDM_TYPE_STRING, eventMatchKey, BinaryOperator.EQ));
        filterConditions.add(new FilterCondition("eventType", FilterCondition.EDM_TYPE_STRING, eventType, BinaryOperator.EQ));
        filterConditions.add(new FilterCondition(Constants.PROCESS_ID, FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ));

        String targetEntityName = Constants.PLANNED_EVENT_ENTITY_NAME;
        return POFUtils.generateUrl(targetEntityName, filterConditions, BinaryOperator.AND, null, null);
    }

    /**
     *
     * @param trackingIdTypes
     * @param partyIds
     * @param deliveryItemId
     * @return the actual routes of the delivery item. (key: altKey of actual route, value: actual route.)
     * get the actual event and corresponding planned event of the delivery item order by actual business time.
     * get the actual spots order by actual time from these events.
     */
    public Map<String, Route> getActualRoute(Set<String> trackingIdTypes, Set<String> partyIds, String deliveryItemId) {
        String url = getActualRouteUrl(deliveryItemId);
        ODataResultList<ProcessEventDirectory> entityList = gttCoreServiceClient.readEntitySetAll(url, ProcessEventDirectory.class);
        return getRoutesFromEvents(trackingIdTypes, partyIds, entityList.getResults());
    }

    private String getActualRouteUrl(String deliveryItemId) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        filterConditions.add(new FilterCondition(Constants.PROCESS_ID, FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ));

        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy(Constants.EVENT_ACTUAL_BUSINESS_TIMESTAMP, ""));

        List<String> expand = new ArrayList<>();
        expand.add(Constants.EVENT_EXPAND);
        expand.add(Constants.PLANNED_EVENT_EXPAND);

        return POFUtils.generateUrlWithCorrelationType(Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME, filterConditions, BinaryOperator.AND, expand, orderbys);
    }


    public Map<String, Route> getRoutesFromEvents(Set<String> trackingIdTypes, Set<String> partyIds, List<ProcessEventDirectory> processEventDirectories) {
        Map<String, Route> map = new HashMap<>();
        for (ProcessEventDirectory processEventDirectory : processEventDirectories) {
            Event event = processEventDirectory.getEvent();

            String altKey = event.getAltKey();
            if (altKey.contains(Constants.RESOURCE)) {
                altKey = altKey.replace(Constants.RESOURCE, Constants.SHIPMENT);
                altKey = altKey.substring(0, altKey.lastIndexOf(':') + 11);
            }

            Route route;
            if (map.containsKey(altKey)) {
                route = map.get(altKey);
            } else {
                route = new Route();
                trackingIdTypes.add(altKey.replaceAll(REGEX_GET_SECOND, REGEX_FIRST_REPLACEMENT));
                partyIds.add(altKey.replaceAll(REGEX_GET_FIRST_PART, REGEX_FIRST_REPLACEMENT));
            }

            ActualSpot actualSpot = generateActualSpotWithPED(processEventDirectory);
            route.addLocationAltKey(actualSpot.getLocationAltKey());
            route.addActualSpot(actualSpot);
            route.setAltKey(altKey);
            route.setGroupId(altKey.replaceAll(REGEX_GET_LAST_PART, REGEX_FIRST_REPLACEMENT));
            map.put(altKey, route);
        }
        return map;
    }

    public ActualSpot generateActualSpotWithPED(ProcessEventDirectory processEventDirectory) {
        Event event = processEventDirectory.getEvent();
        PlannedEvent plannedEvent = processEventDirectory.getPlannedEvent();

        ActualSpot actualSpot = new ActualSpot();
        actualSpot.setAltKey(event.getAltKey());
        actualSpot.setEventId(event.getId());
        actualSpot.setEventTypeOrign(event.getEventType());
        actualSpot.setLatitude(event.getLatitude());
        actualSpot.setLongitude(event.getLongitude());
        actualSpot.setPlannedEventId(processEventDirectory.getPlannedEventId());
        if (plannedEvent != null && !POFUtils.isEventTypeInBlackList(actualSpot.getEventType())) {
            actualSpot.setLocationAltKey(plannedEvent.getLocationAltKey());
            actualSpot.setEventMatchKey(plannedEvent.getEventMatchKey());
        }
        return actualSpot;
    }

    public boolean validLocation(BigDecimal longitude, BigDecimal latitude, Route route, String field) {
        if (longitude == null || latitude == null) {
            switch (field) {
                case PLANNED_LOCATION:
                    route.setPlannedLocationMissing();
                    break;
                case ACTUAL_SPOT:
                    route.setActualSpotsMissing();
                    break;
                default: break;
            }
            return false;
        }
        if (new BigDecimal(Constants.MAX_LONGITUDE).compareTo(longitude.abs()) < 0 || new BigDecimal(Constants.MAX_LATITUDE).compareTo(latitude.abs()) < 0) {
            switch (field) {
                case PLANNED_LOCATION:
                    route.setPlannedLocationInvalid();
                    break;
                case ACTUAL_SPOT:
                    route.setActualSpotsInvalid();
                    break;
                default: break;
            }
            return false;
        }
        return true;
    }

    public void setEventReasonText(List<SideContent> sideContents) {
        sideContents.stream().filter(sideContent -> sideContent.getIsPlannedEvent()&&sideContent.getEventStatusCode().contains("DELAYED")).forEach(sideContent -> {
            UUID plannedEventId = sideContent.getPlannedEventId();
            String url = getLatestReportedDelayEvent(plannedEventId);
            ODataResultList<ProcessEventDirectory> processEventDirectories = gttCoreServiceClient.readEntitySet(url,ProcessEventDirectory.class);
            if(!CollectionUtils.isEmpty(processEventDirectories.getResults())) {
                String eventReasonText = processEventDirectories.getResults().get(0).getEvent().getEventReasonText();
                sideContent.setEventReasonText(eventReasonText);
            }
        });
    }

    public String getLatestReportedDelayEvent(UUID plannedEventId) {
        List<FilterCondition> filterConditions = new ArrayList<>();

        FilterCondition propertyCondition = new FilterCondition("plannedEvent_id", FilterCondition.EDM_TYPE_GUID, plannedEventId.toString(), BinaryOperator.EQ);
        filterConditions.add(propertyCondition);

        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy(Constants.EVENT_ACTUAL_BUSINESS_TIMESTAMP, "desc"));

        String targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        String filter = "and (substringof('Delay',event/eventType)) ";
        List<String> expand = new ArrayList<>();
        expand.add("event");
        return POFUtils.generateUrl(targetEntityName, filter, filterConditions, BinaryOperator.AND, false, false, expand, orderbys)+"&$top=1";
    }

    public void setLocationInSideContent(List<SideContent> sideContents) {
        if (CollectionUtils.isEmpty(sideContents)) {
            return;
        }
        Set<String> locationAltKeys = sideContents.stream().map(SideContent::getLocationAltKey).collect(Collectors.toSet());
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, Location> locationMap = createLocationMap(locations);
        sideContents.forEach(sideContent -> {
            Optional<Location> location = Optional.ofNullable(locationMap.get(sideContent.getLocationAltKey()));
            location.ifPresent(l ->
                    sideContent.setLocation(getLocationDetail(l))
            );
        });
    }

    public List<PlannedEvent> removeNotMatchedEvents(List<PlannedEvent> plannedEvents, String plannedEventId) {
        int lastIndex = 0;
        for (; lastIndex < plannedEvents.size(); lastIndex++) {
            if (StringUtils.equals(plannedEvents.get(lastIndex).getId().toString(), plannedEventId)) {
                break;
            }
        }
        if(plannedEvents.size() == 0) return plannedEvents;
        return plannedEvents.subList(0, lastIndex + 1);
    }

    private LocationDTO getLocationDTO(String locationAltKey) {
        Location location = gttCoreServiceClient.getLocation(locationAltKey);
        return getLocationDetail(location);
    }

    /***
     * Get planned routes for delivery item
     * @param deliveryItemId Id of delivery item
     * @return Routes for different shipments (key: groupId,as the same as first 10 characters of eventMatchKey)
     * get the planned events of the delivery item
     * group the planned events by shipment id(first 10 characters of event match key)
     *
     */
    public Map<String, Route> getPlannedRoutes(String deliveryItemId) {
        Map<String, Route> plannedRouteMap = new HashMap<>();
        List<PlannedEvent> plannedEvents = getPlannedEvents4DeliveryItem(deliveryItemId);

        // Group the planned events by shipment id (first 10 characters of eventMatchKey)
        Map<String, List<PlannedEvent>> eventMap = plannedEvents.stream()
                .filter(plannedEvent -> StringUtils.isNotEmpty(plannedEvent.getEventMatchKey()))
                .collect(Collectors.groupingBy(plannedEvent -> StringUtils.substring(plannedEvent.getEventMatchKey(), 0, SHIPMENT_ID_LENGTH)));
        //get all the locations from location master.
        Set<String> locationAltKeys = plannedEvents.stream().map(PlannedEvent::getLocationAltKey).collect(Collectors.toSet());
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, Location> locationMap = createLocationMap(locations);
        //get the sorted planned route with location.
        eventMap.forEach((eventMatchKey, plnEvents) -> {
            Route plannedRoute = generatePlannedRoute(plnEvents, locationMap);
            String shipmentId = eventMatchKey.substring(0, SHIPMENT_ID_LENGTH);
            plannedRoute.setGroupId(shipmentId);
            plannedRouteMap.put(shipmentId, plannedRoute);
        });

        return plannedRouteMap;
    }

    private List<PlannedEvent> getPlannedEvents4DeliveryItem(String deliveryItemId) {
        return pofService.getPlannedEvents4TP(UUID.fromString(deliveryItemId));
    }

    private Map<String, Location> createLocationMap(List<Location> locations) {
        Map<String, Location> locationMap = new HashMap<>();
        locations.forEach(location -> locationMap.put(location.getLocationAltKey(), location));
        return locationMap;
    }
    public Comparator<PlannedEvent> getPlannedEventComparator() {
        return comparing(PlannedEvent::getPlannedBusinessTimestamp, nullsFirst(Long::compareTo))
                .thenComparing(PlannedEvent::getPayloadSequence, nullsFirst(Integer::compareTo));
    }

    /**
     *
     * @param plannedEvents
     * @param locationMap
     * @return sorted planned route with location
     * Group the planned events by eventMatchKey (shipment id + stop id), then sort by eventMatchKey
     * sort the each group with planned business timestamp and payloadSequence
     * set all location of the planned spot.
     */
    private Route generatePlannedRoute(List<PlannedEvent> plannedEvents, Map<String, Location> locationMap) {
        Route route = new Route();
        List<PlannedSpot> spots = new LinkedList<>();
        // Group the planned events by eventMatchKey (shipment id + stop id), then sort by eventMatchKey
        plannedEvents.stream()
                .collect(Collectors.groupingBy(PlannedEvent::getEventMatchKey))
                .entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .forEach(map -> {
                    List<PlannedEvent> events   = map.getValue();
                    events.stream().sorted(getPlannedEventComparator()).forEach(event -> {
                        Optional<Location> location = Optional.ofNullable(locationMap.get(event.getLocationAltKey()));
                        location.ifPresent(l -> {
                            BigDecimal longitude = l.getLongitude();
                            BigDecimal latitude = l.getLatitude();
                            String locationDescription = l.getLocationDescription();
                            String locationType = l.getObjectTypeCode();
                            UUID eventId = event.getId();
                            String eventMatchKey = event.getEventMatchKey();
                            String eventType = event.getEventType();
                            String eventStatusCode = event.getEventStatusCode();
                            String locationAltKey = event.getLocationAltKey();
                            Long plannedBusinessTimestamp = event.getPlannedBusinessTimestamp();
                            PlannedSpot spot = new PlannedSpot();
                            spot.setLongitude(longitude);
                            spot.setLatitude(latitude);
                            spot.setObjectTypeCode(locationType);
                            spot.setEventId(eventId);
                            spot.setEventMatchKey(eventMatchKey);
                            spot.setLocationDescription(locationDescription);
                            spot.setEventTypeOrign(eventType);
                            spot.setPlannedEventId(eventId);
                            spot.setEventStatusCode(eventStatusCode);
                            spot.setLocationAltKey(locationAltKey);
                            spot.setPlannedBusinessTimestamp(POFUtils.getTimeStr(plannedBusinessTimestamp));
                            spots.add(spot);
                        });
                    });
                });

        route.setPlannedSpots(spots);
        return route;
    }

}
