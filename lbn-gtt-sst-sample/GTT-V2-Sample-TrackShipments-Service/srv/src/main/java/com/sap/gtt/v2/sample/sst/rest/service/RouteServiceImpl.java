package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.odata.service.StopsForVpService;
import com.sap.gtt.v2.sample.sst.rest.model.*;
import com.sap.gtt.v2.sample.sst.rest.validator.RouteValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

import javax.validation.constraints.NotNull;
import java.util.*;
import java.util.function.Function;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.isPlannedEventNotBeenReported;
import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsLast;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * @author Aliaksandr Miron
 */

@Validated
@Service
public class RouteServiceImpl implements RouteService {

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    @Autowired
    private RouteValidator routeValidator;

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private StopsForVpService stopsForVpService;

    @Autowired
    private PlannedSpotService plannedSpotService;

    @Autowired
    private ActualSpotService actualSpotService;

    @Autowired
    private CurrentLocationService currentLocationService;

    @Override
    public Route getByShipmentId(@NotNull final String shipmentId) {
        final Route route = new Route();
        final List<PlannedEvent> rawPlannedEvents = getPlannedEvents(shipmentId);
        fillAndValidateStopsForVp(route, shipmentId);
        fillAndValidateActualSpots(route, shipmentId);
        fillCurrentLocation(route);
        fillAndValidatePlannedSpots(route, rawPlannedEvents);
        updateRoutesConnection(route, rawPlannedEvents);
        updateETAOfStopsForVp(route);
        return route;
    }

    private void fillAndValidateStopsForVp(final Route route, final String shipmentId) {
        final List<StopsForVp> stopsForVp = stopsForVpService.getAll(shipmentId);
        final List<StopsForVp> filteredStopsForVp = filterOutStopsForVpWithInvalidCoordinates(stopsForVp);
        routeValidator.validateStopsForVp(route, stopsForVp);
        route.setStopsForVp(filteredStopsForVp);
    }

    private void fillAndValidateActualSpots(final Route route, final String shipmentId) {
        final List<ProcessEventDirectory> actualEvents = getActualEventsInWhitelist(shipmentId);
        final List<ActualSpot> actualSpots = actualSpotService.getAllAscending(actualEvents);
        final List<ActualSpot> filteredActualSpots = filterOutActualSpotsWithInvalidCoordinates(actualSpots);
        routeValidator.validateActualSpots(route, actualSpots);
        route.setActualSpots(filteredActualSpots);
    }

    private List<ProcessEventDirectory> getActualEventsInWhitelist(final String shipmentId) {
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByShipmentId(shipmentId);
        return ProcessEventDirectoryUtils.filterByWhitelistForRoutes(processEventDirectories);
    }

    private void fillCurrentLocation(final Route route) {
        final List<ActualSpot> actualSpots = route.getActualSpots();
        final Optional<CurrentLocation> currentLocation = currentLocationService.getFromActualSpots(actualSpots);
        currentLocation.ifPresent(route::setCurrentLocation);
    }

    private List<ActualSpot> filterOutActualSpotsWithInvalidCoordinates(final List<ActualSpot> actualSpots) {
        return actualSpots.stream()
                .filter(actualSpot -> coordinatesValidator.isValid(actualSpot.getLongitude(), actualSpot.getLatitude()))
                .collect(toList());
    }

    private List<StopsForVp> filterOutStopsForVpWithInvalidCoordinates(final List<StopsForVp> stopsForVp) {
        return stopsForVp.stream()
                .filter(stopForVp -> nonNull(stopForVp.getLocation()))
                .filter(stopForVp -> coordinatesValidator.isValid(
                        stopForVp.getLocation().getLongitude(),
                        stopForVp.getLocation().getLatitude()))
                .collect(toList());
    }

    private List<PlannedEvent> getPlannedEvents(final String shipmentId) {
        List<PlannedEvent> plannedEvents = plannedEventService.getAllByShipmentId(shipmentId);
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

    private void fillAndValidatePlannedSpots(final Route route, final List<PlannedEvent> plannedEvents) {
        final List<PlannedSpot> plannedSpots = plannedSpotService.getAll(plannedEvents);
        final List<PlannedSpot> filteredPlannedSpots = filterOutPlannedSpotsWithConditions(plannedSpots);
        routeValidator.validatePlannedSpots(route, plannedSpots);
        route.setPlannedSpots(filteredPlannedSpots);
    }

    private List<PlannedSpot> filterOutPlannedSpotsWithConditions(final List<PlannedSpot> plannedSpots) {
        return plannedSpots.stream()
                .filter(plannedSpot -> coordinatesValidator.isValid(plannedSpot.getLongitude(), plannedSpot.getLatitude()))
                .filter(plannedSpot -> isPlannedEventNotBeenReported(plannedSpot.getEventStatusCode()))
                .collect(toList());
    }

    private void updateRoutesConnection(final Route route, final List<PlannedEvent> plannedEvents) {
        CurrentLocation currentLocation = route.getCurrentLocation();
        List<PlannedSpot> plannedSpots = route.getPlannedSpots();
        if (nonNull(currentLocation)) {
            PlannedEvent plannedEventOfCurrentLocation = currentLocation.getPlannedEvent();
            PlannedSpot nextPlannedSpot = plannedSpotService.findNextPlannedSpot(currentLocation, plannedSpots, plannedEvents);
            if (nonNull(nextPlannedSpot)) {
                updatePlannedSpots(plannedSpots, nextPlannedSpot);
                route.setToConnectPlannedAndActualRoute(true);
            } else if (nonNull(plannedEventOfCurrentLocation)
                    && plannedEventOfCurrentLocation.getId().compareTo(plannedEvents.get(plannedEvents.size() - 1).getId()) == 0) {
                plannedSpots.clear();
            }
        }
    }

    private void updatePlannedSpots(final List<PlannedSpot> plannedSpots, final PlannedSpot nextPlannedSpot) {
        Iterator<PlannedSpot> it = plannedSpots.iterator();
        while (it.hasNext()) {
            PlannedSpot plannedSpot = it.next();
            if (plannedSpot.getEventId().compareTo(nextPlannedSpot.getEventId()) != 0) {
                it.remove();
            } else {
                return;
            }
        }
    }

    private void updateETAOfStopsForVp(final Route route) {
        List<StopsForVp> stopsForVp = route.getStopsForVp();
        Optional<CurrentLocation> currentLocationOpt = Optional.ofNullable(route.getCurrentLocation());

        currentLocationOpt.ifPresent(currentLocation -> {
            List<EstimatedArrival> estimatedArrivals = currentLocation.getEstimatedArrival();
            Map<String, EstimatedArrival> stopEstimatedArrival = estimatedArrivals.stream()
                    .collect(toMap(EstimatedArrival::getStopId, Function.identity()));
            stopsForVp.forEach(stopForVp -> stopForVp.setEstimatedArrival(stopEstimatedArrival.get(stopForVp.getStopId())));
        });
    }
}
