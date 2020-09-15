package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.*;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.sof.odata.model.Shipment;
import com.sap.gtt.v2.sample.sof.odata.model.StopsForVP;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.*;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class MapService {
    public static final String ACTUAL_SPOT = "ActualSpot";
    public static final String PLANNED_LOCATION = "PlannedLocation";
    private static final String replacement = "$1";
    private final static String regex_getTrackingIdTypes = ".+:([^:].+):[^:].+$";
    private final static String regex_getPartyIds = "(.+):[^:].+:[^:].+$";
    private final static String regex_getTPId = ".+:([^:].+)$";
    private static final int SHIPMENT_ID_LENGTH = 10;
    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;
    @Autowired
    private SOFService sofService;

    public List<Route> getRoutes(String deliveryItemId) {
        Set<String> trackingIdTypes = new HashSet<>();
        Set<String> partyIds = new HashSet<>();
        Map<String, Route> actualRoutes = getActualRoute(trackingIdTypes, partyIds, deliveryItemId);
        setLocationInActualSpot(actualRoutes);
        getCurrentLocations(actualRoutes);
        getNextStops(actualRoutes, deliveryItemId);
        Map<String, Route> plannedRoutes = getPlannedRoutes(deliveryItemId);
        List<Route> result = mergeRoutes(trackingIdTypes, partyIds, actualRoutes, plannedRoutes);
        getTransPortationModeAndStops(result);
        setETA(result);
        setDestination(result, deliveryItemId);
        removeReportedPlannedEvent(result);
        fillCoordinates(result);
        removeInvalidCoordinates(result);
        setLocationType4ActualSpot(result);
        return result;
    }

    private void removeReportedPlannedEvent(List<Route> result) {
        result.forEach(route -> route.getPlannedSpots().removeIf(plannedSpot -> !SOFUtils.isEventStatusInWhiteList(plannedSpot.getEventStatusCode())));
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
                String url = Constants.URL_SPLITTER + k + "?$filter=" + filter.getExpressionString();
                List<EventEx> events = gttCoreServiceClient.readEntitySet(url, EventEx.class).getResults();
                events.forEach(eventEx -> eventExMap.put(eventEx.getId(), eventEx));
            }
        });
        return eventExMap;
    }

    public void createEventTypeMap(List<Route> result, Map<String, Set<UUID>> eventTypeMaps) {
        if(CollectionUtils.isEmpty(result)) {
            return;
        }
        result.forEach(route -> {
                    if (!CollectionUtils.isEmpty(route.getActualSpots())) {
                        createEventTypeMap(eventTypeMaps, route);
                    }
                }
        );
    }

    public void createEventTypeMap(Map<String, Set<UUID>> eventTypeMaps, Route route) {
        route.getActualSpots().forEach(actualSpot -> {
            if (SOFUtils.isEventTypeInWhiteList(actualSpot.getEventType())) {
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
        String url = "/DeliveryItem(guid'" + deliveryItemId + "')";
        DeliveryItem deliveryItem = gttCoreServiceClient.readEntity(url, DeliveryItem.class);
        String destinationAltKey = SOFUtils.generateLocationAltKey(deliveryItem.getPartyId(), deliveryItem.getLogicalSystem(), deliveryItem.getDestinationLocationTypeCode(), deliveryItem.getDestination());
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


    private void setLocationInActualSpot(Map<String, Route> actualRoutes) {
        List<Route> routes = new ArrayList<>(actualRoutes.values());
        Set<String> locationAltKeys = new HashSet<>();
        routes.forEach(route -> locationAltKeys.addAll(route.getLocationAltKey()));
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, Location> locationMap = createLocationMap(locations);
        routes.forEach(route -> {
            route.getActualSpots().forEach(actualSpot -> {
                Location location = locationMap.get(actualSpot.getLocationAltKey());
                LocationDTO locationDTO = getLocationDetail(location);
                actualSpot.setLocation(locationDTO);
                if (locationDTO != null) {
                    actualSpot.setLocationDescription(locationDTO.getLocationDescription());
                    actualSpot.setObjectTypeCode(locationDTO.getObjectTypeCode());
                }
            });
        });
    }

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

    private void getTransPortationModeAndStops(List<Route> result) {
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
            route.setStopsForVPS(new ArrayList<>());
        }
    }

    private Set<String> generateLocationAltKeyWithStopsForVP(Shipment shipment, List<StopsForVP> stops) {
        stops.forEach(stop -> {
            String locationAltKey = SOFUtils.generateLocationAltKey(shipment.getPartyId(), shipment.getLogicalSystem(), stop.getLocationTypeCode(), stop.getLocationId());
            stop.setLocationAltKey(locationAltKey);
        });
        Set<String> locationAltKeys = stops.stream().map(StopsForVP::getLocationAltKey).collect(Collectors.toSet());
        return locationAltKeys;
    }

    private Shipment getShipment(String altKey) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("altKey", FilterCondition.EDM_TYPE_STRING, altKey, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        List<String> expand = new ArrayList<>();
        expand.add("stopsForVP");
        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy("stopsForVP/ordinalNo", ""));
        String url = SOFUtils.generateUrl("Shipment", filterConditions, BinaryOperator.AND, expand, null);
        ODataResultList<Shipment> resultList = gttCoreServiceClient.readEntitySet(url, Shipment.class);
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
        ODataResultList<EventEx> events = gttCoreServiceClient.readEntitySet(url, EventEx.class);
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
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("id", FilterCondition.EDM_TYPE_GUID, eventId.toString(), BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        List<String> expand = null;
        if (SOFUtils.isEventTypeWithEstimatedArrival(eventType)) {
            expand = new ArrayList<>();
            expand.add("estimatedArrival");
        }
        eventType = eventType.substring(eventType.lastIndexOf(".") + 1);
        String targetEntityName = eventType;
        return SOFUtils.generateUrl(targetEntityName, filterConditions, BinaryOperator.AND, expand, null);
    }

    public List<Route> mergeRoutes(Set<String> trackingIdTypes, Set<String> partyIds, Map<String, Route> actualRoutes, Map<String, Route> plannedRoutes) {
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

    public void groupEventsWithGroupId(Map<String, Route> actualRoutes, List<Route> plannedR, String k, Route plannedRoute, String partyId, String trackingIdType) {
        String altKey = partyId.concat(":").concat(trackingIdType).concat(":").concat(k);
        if (actualRoutes.containsKey(altKey)) {
            Route actualRoute = actualRoutes.get(altKey);
            UUID nextPlannedEventId = actualRoute.getNextStopEvent() != null ? actualRoute.getNextStopEvent().getId() : null;
            UUID lastReportedPlannedEventId = actualRoute.getActualSpots().get(actualRoute.getActualSpots().size() - 1).getPlannedEventId();
            if (pEInPG(nextPlannedEventId, plannedRoute)) {
                Route route = actualRoutes.get(altKey);
                List<PlannedSpot> plannedSpots = getUnReportedPlannedSpots(nextPlannedEventId, plannedRoute.getPlannedSpots(), true);
                route.addPlannedSpots(plannedSpots);
                actualRoutes.put(altKey, route);

            } else if (pEInPG(lastReportedPlannedEventId, plannedRoute)) {
                Route route = actualRoutes.get(altKey);
                List<PlannedSpot> plannedSpots = getUnReportedPlannedSpots(lastReportedPlannedEventId, plannedRoute.getPlannedSpots(), false);
                route.addPlannedSpots(plannedSpots);
                actualRoutes.put(altKey, route);
            }
            plannedR.remove(plannedRoute);
        }
    }

    private List<PlannedSpot> getUnReportedPlannedSpots(UUID plannedEventId, List<PlannedSpot> plannedSpots, boolean contains) {
        if (plannedEventId == null) {
            return new ArrayList<>();
        }
        List<PlannedSpot> result = new ArrayList<>();
        boolean flag = false;
        for (int index = 0; index < plannedSpots.size(); index++) {
            if (plannedEventId.equals(plannedSpots.get(index).getEventId())) {
                flag = true;
                if (contains) {
                    result.add(plannedSpots.get(index));
                }
            } else if (flag) {
                result.add(plannedSpots.get(index));
            }
        }
        return result;
    }

    //determine if planned event in planned group.
    public boolean pEInPG(UUID plannedEventId, Route plannedRoute) {
        if (plannedEventId == null) {
            return false;
        }
        for (PlannedSpot spot : plannedRoute.getPlannedSpots()) {
            if (plannedEventId.equals(spot.getEventId())) {
                return true;
            }
        }
        return false;
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
        ODataResultList<PlannedEvent> entitySet = gttCoreServiceClient.readEntitySet(url, PlannedEvent.class);
        if (entitySet.getResults().isEmpty()) {
            return null;
        }
        return entitySet.getResults().get(0);
    }

    private String generatePlannedEventUrl(String eventMatchKey, String eventType, String deliveryItemId) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("eventMatchKey", FilterCondition.EDM_TYPE_STRING, eventMatchKey, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        propertyCondition = new FilterCondition("eventType", FilterCondition.EDM_TYPE_STRING, eventType, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        String targetEntityName = Constants.PLANNED_EVENT_ENTITY_NAME;
        return SOFUtils.generateUrl(targetEntityName, filterConditions, BinaryOperator.AND, null, null);
    }

    public Map<String, Route> getActualRoute(Set<String> trackingIdTypes, Set<String> partyIds, String deliveryItemId) {
        String url = getActualRouteUrl(deliveryItemId);
        ODataResultList<ProcessEventDirectory> entityList = gttCoreServiceClient.readEntitySet(url, ProcessEventDirectory.class);
        List<ProcessEventDirectory> processEventDirectories = entityList.getResults();
        return getRoutesFromEvents(trackingIdTypes, partyIds, processEventDirectories);
    }

    private String getActualRouteUrl(String deliveryItemId) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy("event/actualBusinessTimestamp", ""));
        String targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        List<String> expand = new ArrayList<>();
        expand.add(Constants.EVENT_EXPAND);
        expand.add(Constants.PLANNED_EVENT_EXPAND);
        return SOFUtils.generateUrlWithCorrelationType(targetEntityName, filterConditions, BinaryOperator.AND, expand, orderbys);
    }

    public Map<String, Route> getRoutesFromEvents(Set<String> trackingIdTypes, Set<String> partyIds, List<ProcessEventDirectory> processEventDirectories) {
        Map<String, Route> map = new HashMap<>();
        for (ProcessEventDirectory processEventDirectory : processEventDirectories) {
            Event event = processEventDirectory.getEvent();

            Route route;
            String altKey = event.getAltKey();
            if (altKey.contains(Constants.RESOURCE)) {
                altKey = altKey.replace(Constants.RESOURCE, Constants.SHIPMENT);
                altKey = altKey.substring(0, altKey.lastIndexOf(":") + 11);
            }
            if (map.containsKey(altKey)) {
                route = map.get(altKey);
            } else {
                route = new Route();
                trackingIdTypes.add(altKey.replaceAll(regex_getTrackingIdTypes, replacement));
                partyIds.add(altKey.replaceAll(regex_getPartyIds, replacement));
            }
            ActualSpot actualSpot = generateActualSpotWithPED(processEventDirectory);
            route.addLocationAltKey(actualSpot.getLocationAltKey());
            route.addActualSpot(actualSpot);
            route.setAltKey(altKey);
            route.setGroupId(altKey.replaceAll(regex_getTPId, replacement));
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
        if (plannedEvent != null) {
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
            }
            return false;
        }
        return true;
    }

    public List<SideContent> getSideContents(String deliveryItemId, String altKey, String eventMatchKey, String plannedEventId) {
        List<SideContent> actualEventInsideContents = new ArrayList<>();
        if (StringUtils.isNotBlank(altKey)) {
            actualEventInsideContents = getSideContentsInActualEvent(deliveryItemId, altKey);
        }
        List<SideContent> sideContents = new ArrayList<>();
        if (StringUtils.isNotBlank(eventMatchKey) && StringUtils.isNotBlank(plannedEventId)) {
            sideContents = getSideContentsInPlannedEvents(deliveryItemId, eventMatchKey, plannedEventId);
        }
        sideContents.addAll(actualEventInsideContents);
        setLocationInSideContent(sideContents);
        return sideContents;
    }

    public void removeReportedPlannedEventInSideContent(List<PlannedEvent> plannedEvents) {
        plannedEvents.removeIf(plannedEvent -> !SOFUtils.isEventStatusInWhiteList(plannedEvent.getEventStatusCode()));
    }


    public List<SideContent> getSideContentsInPlannedEvents(String deliveryItemId, String eventMatchKey, String plannedEventId) {
        List<PlannedEvent> plannedEvents = getPlannedEvent4SideContent(deliveryItemId, eventMatchKey, plannedEventId);
        removeReportedPlannedEventInSideContent(plannedEvents);
        return getSideContentsFromPlannedEvents(plannedEvents);
    }

    public List<SideContent> getSideContentsInActualEvent(String deliveryItemId, String altKey) {
        String url = generateContentSideUrl(deliveryItemId, altKey);
        ODataResultList<ProcessEventDirectory> entityList = gttCoreServiceClient.readEntitySet(url, ProcessEventDirectory.class);
        List<ProcessEventDirectory> processEventDirectories = entityList.getResults();
        return getSideContentsFromPED(processEventDirectories);
    }

    public List<SideContent> getSideContentsFromPlannedEvents(List<PlannedEvent> plannedEvents) {
        List<SideContent> sideContents = new LinkedList<>();
        plannedEvents.forEach(plannedEvent -> {
            SideContent sideContent = new SideContent();
            sideContent.setEventMatchKey(plannedEvent.getEventMatchKey());
            sideContent.setEventTypeFullName(plannedEvent.getEventType());
            sideContent.setEventType(plannedEvent.getEventType().substring(plannedEvent.getEventType().lastIndexOf(".") + 1));
            sideContent.setIsActualEvent(false);
            sideContent.setIsPlannedEvent(true);
            sideContent.setLocationAltKey(plannedEvent.getLocationAltKey());
            sideContent.setPlannedEventId(plannedEvent.getId());
            sideContent.setEventStatusCode(plannedEvent.getEventStatusCode());
            sideContent.setPlannedBusinessTimestamp(SOFUtils.getTimeStr(plannedEvent.getPlannedBusinessTimestamp()));
            sideContents.add(sideContent);
        });
        return sideContents;
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

    public List<PlannedEvent> getPlannedEvent4SideContent(String deliveryItemId, String eventMatchKey, String plannedEventId) {
        List<PlannedEvent> plannedEvents = getPlannedEventWithTPIdAndEventMatchKey(deliveryItemId, eventMatchKey);
        return removeNotMatchedEvents(plannedEvents, plannedEventId);
    }

    public List<PlannedEvent> removeNotMatchedEvents(List<PlannedEvent> plannedEvents, String plannedEventId) {
        int lastIndex = 0;
        for (; lastIndex < plannedEvents.size(); lastIndex++) {
            if (StringUtils.equals(plannedEvents.get(lastIndex).getId().toString(), plannedEventId)) {
                break;
            }
        }
        if(plannedEvents.size()==0) return plannedEvents;
        return plannedEvents.subList(0, lastIndex + 1);
    }

    public List<PlannedEvent> getPlannedEventWithTPIdAndEventMatchKey(String deliveryItemId, String eventMatchKey) {
        String url = generatePlannedEventUrl(deliveryItemId, eventMatchKey);
        ODataResultList<PlannedEvent> oDataResultList = gttCoreServiceClient.readEntitySet(url, PlannedEvent.class);
        return oDataResultList.getResults();
    }

    public String generatePlannedEventUrl(String deliveryItemId, String eventMatchKey) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy("eventMatchKey", "desc"));
        orderbys.add(new OrderBy("plannedBusinessTimestamp", "desc"));
        orderbys.add(new OrderBy("payloadSequence", "desc"));
        String targetEntityName = Constants.PLANNED_EVENT_ENTITY_NAME;
        String filter = "and (substringof('" + eventMatchKey + "',eventMatchKey)) ";
        return SOFUtils.generateUrl(targetEntityName, filter, filterConditions, BinaryOperator.AND, false, false, null, orderbys);
    }

    public String generateContentSideUrl(String deliveryItemId, String altKey) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, deliveryItemId, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        List<OrderBy> orderbys = new ArrayList<>();
        orderbys.add(new OrderBy("event/actualBusinessTimestamp", "desc"));
        String targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        List<String> expand = new ArrayList<>();
        expand.add(Constants.EVENT_EXPAND);
        expand.add(Constants.PLANNED_EVENT_EXPAND);
        if (altKey.contains(Constants.SHIPMENT)) {
            String resourceAltKey = altKey.replace(Constants.SHIPMENT, Constants.RESOURCE);
            String filter = "and (substringof('" + resourceAltKey + "',event/altKey) or (event/altKey eq '" + altKey + "')) ";
            return SOFUtils.generateUrl(targetEntityName, filter, filterConditions, BinaryOperator.AND, false, true, expand, orderbys);
        } else {
            propertyCondition = new FilterCondition("event/altKey", FilterCondition.EDM_TYPE_STRING, altKey, BinaryOperator.EQ);
            filterConditions.add(propertyCondition);
            return SOFUtils.generateUrlWithCorrelationTypeAndNoGeo(targetEntityName, filterConditions, BinaryOperator.AND, expand, orderbys);
        }
    }

    public List<SideContent> getSideContentsFromPED(List<ProcessEventDirectory> processEventDirectories) {
        List<SideContent> sideContents = new ArrayList<>();
        Set<UUID> plannedEventIds = new HashSet<>();
        for (ProcessEventDirectory processEventDirectory : processEventDirectories) {
            UUID plannedEventId = processEventDirectory.getPlannedEventId();
            if (plannedEventId != null && plannedEventIds.contains(plannedEventId)) {
                continue;
            }
            if (plannedEventId != null) {
                plannedEventIds.add(plannedEventId);
            }
            getSideContent(plannedEventId, sideContents, processEventDirectory);
        }
        return sideContents;
    }

    private void getSideContent(UUID plannedEventId, List<SideContent> sideContents, ProcessEventDirectory processEventDirectory) {
        Event event = processEventDirectory.getEvent();
        PlannedEvent plannedEvent = processEventDirectory.getPlannedEvent();
        SideContent sideContent = new SideContent();
        String actualTime = SOFUtils.getTimeStr(event.getActualBusinessTimestamp());
        sideContent.setActualBusinessTimestamp(actualTime);
        sideContent.setLocationAltKey(event.getLocationAltKey());
        String eventType = event.getEventType();
        if (StringUtils.isNotBlank(eventType)) {
            eventType = eventType.substring(eventType.lastIndexOf(".") + 1);
        }
        sideContent.setEventType(eventType);
        sideContent.setIsPlannedEvent(false);
        sideContent.setIsActualEvent(true);
        sideContent.setEventMatchKey(event.getEventMatchKey());
        if (plannedEventId != null && plannedEvent != null) {
            sideContent.setPlannedEventId(plannedEventId);
            sideContent.setEventStatusCode(plannedEvent.getEventStatusCode());
            sideContent.setLocationAltKey(plannedEvent.getLocationAltKey());
            eventType = plannedEvent.getEventType();
            sideContent.setEventTypeFullName(eventType);
            if (StringUtils.isNotBlank(eventType)) {
                eventType = eventType.substring(eventType.lastIndexOf(".") + 1);
            }
            sideContent.setEventType(eventType);
            sideContent.setEventMatchKey(plannedEvent.getEventMatchKey());
            String plannedTime = SOFUtils.getTimeStr(plannedEvent.getPlannedBusinessTimestamp());
            sideContent.setPlannedBusinessTimestamp(plannedTime);
            sideContent.setIsPlannedEvent(true);
        }
        sideContents.add(sideContent);
    }

    private LocationDTO getLocationDTO(String locationAltKey) {
        Location location = gttCoreServiceClient.getLocation(locationAltKey);
        return getLocationDetail(location);
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


    /***
     * Get planned routes for delivery item
     * @param deliveryItemId Id of delivery item
     * @return Routes for different shipments
     */
    public Map<String, Route> getPlannedRoutes(String deliveryItemId) {
        Map<String, Route> plannedRouteMap = new HashMap<>();
        List<PlannedEvent> plannedEvents = getPlannedEvents4DeliveryItem(deliveryItemId);

        // Group the planned events by shipment id (first 10 characters of eventMatchKey)
        Map<String, List<PlannedEvent>> eventMap = plannedEvents.stream()
                .filter(plannedEvent -> StringUtils.isNotEmpty(plannedEvent.getEventMatchKey()))
                .collect(Collectors.groupingBy(plannedEvent -> StringUtils.substring(plannedEvent.getEventMatchKey(), 0, SHIPMENT_ID_LENGTH)));

        Set<String> locationAltKeys = plannedEvents.stream().map(PlannedEvent::getLocationAltKey).collect(Collectors.toSet());
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, Location> locationMap = createLocationMap(locations);

        eventMap.forEach((eventMatchKey, plnEvents) -> {
            Route plannedRoute = generatePlannedRoute(plnEvents, locationMap);
            String shipmentId = eventMatchKey.substring(0, SHIPMENT_ID_LENGTH);
            plannedRoute.setGroupId(shipmentId);
            plannedRouteMap.put(shipmentId, plannedRoute);
        });

        return plannedRouteMap;
    }

    private List<PlannedEvent> getPlannedEvents4DeliveryItem(String deliveryItemId) {
        return sofService.getPlannedEvents4TP(UUID.fromString(deliveryItemId));
    }

    private Map<String, Location> createLocationMap(List<Location> locations) {
        Map<String, Location> locationMap = new HashMap<>();
        locations.forEach(location -> locationMap.put(location.getLocationAltKey(), location));
        return locationMap;
    }

    private Route generatePlannedRoute(List<PlannedEvent> plannedEvents, Map<String, Location> locationMap) {
        Route route = new Route();
        List<PlannedSpot> spots = new LinkedList<>();
        // Group the planned events by eventMatchKey (shipment id + stop id), then sort by eventMatchKey
        plannedEvents.stream().collect(Collectors.groupingBy(PlannedEvent::getEventMatchKey))
                .entrySet().stream().sorted(Map.Entry.comparingByKey())
                .forEach(map -> {
                    List<PlannedEvent> events = map.getValue();
                    events.stream().sorted((p1, p2) -> {
                                Long p1ts = p1.getPlannedBusinessTimestamp() == null ? Long.MIN_VALUE : p1.getPlannedBusinessTimestamp();
                                Long p2ts = p2.getPlannedBusinessTimestamp() == null ? Long.MIN_VALUE : p2.getPlannedBusinessTimestamp();
                                if (p1ts.compareTo(p2ts) != 0) {
                                    return p1.getPlannedBusinessTimestamp().compareTo(p2.getPlannedBusinessTimestamp());
                                }
                                return p1.getPayloadSequence().compareTo(p2.getPayloadSequence());
                            }
                    ).forEach(event -> {
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
                            spot.setPlannedBusinessTimestamp(SOFUtils.getTimeStr(plannedBusinessTimestamp));
                            spots.add(spot);

                        });
                    });
                });

        route.setPlannedSpots(spots);
        return route;
    }

}
