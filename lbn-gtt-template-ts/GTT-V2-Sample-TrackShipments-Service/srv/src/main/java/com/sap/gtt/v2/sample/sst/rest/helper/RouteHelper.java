package com.sap.gtt.v2.sample.sst.rest.helper;

import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventType.ARRIVAL;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toMap;

import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link RouteHelper} is a helper class for {@link Route} entity.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class RouteHelper {

    @Autowired
    private PlannedSpotHelper plannedSpotHelper;

    /**
     * Updates actual and planned routes connection.
     *
     * @param route         - {@link Route} entity of tracked process
     * @param plannedEvents - list of {@link PlannedEvent} of tracked process
     */
    public void updateRoutesConnection(@NotNull final Route route, @NotNull final List<PlannedEvent> plannedEvents) {
        final CurrentLocation currentLocation = route.getCurrentLocation();
        final List<PlannedSpot> plannedSpots = route.getPlannedSpots();
        if (nonNull(currentLocation)) {
            final PlannedEvent plannedEventOfCurrentLocation = currentLocation.getPlannedEvent();
            final Optional<PlannedSpot> nextPlannedSpotOpt = plannedSpotHelper.findNextPlannedSpot(currentLocation, plannedSpots, plannedEvents);
            if (nextPlannedSpotOpt.isPresent()) {
                final PlannedSpot nextPlannedSpot = nextPlannedSpotOpt.get();
                updatePlannedSpots(plannedSpots, nextPlannedSpot);
                route.setToConnectPlannedAndActualRoute(true);
            } else if (nonNull(plannedEventOfCurrentLocation)
                    && plannedEventOfCurrentLocation.getId().compareTo(plannedEvents.get(plannedEvents.size() - 1).getId()) == 0) {
                plannedSpots.clear();
            }
        }
    }

    /**
     * Updates ETA data of {@link StopsForVp} of {@link Route} entity.
     *
     * @param route - {@link Route} entity of tracked process
     */
    public void updateETAOfStopsForVp(@NotNull final Route route) {
        final List<StopsForVp> stopsForVp = route.getStopsForVp();
        final Optional<CurrentLocation> currentLocationOpt = Optional.ofNullable(route.getCurrentLocation());
        currentLocationOpt.ifPresent(currentLocation -> {
            final List<EstimatedArrival> estimatedArrivals = currentLocation.getEstimatedArrival();
            final Map<String, EstimatedArrival> stopEstimatedArrival = estimatedArrivals.stream()
                    .collect(toMap(EstimatedArrival::getStopId, Function.identity()));
            stopsForVp.forEach(stopForVp -> stopForVp.setEstimatedArrival(stopEstimatedArrival.get(stopForVp.getStopId())));
        });
    }

    /**
     * Updates event status of {@link StopsForVp} of {@link Route} entity.
     *
     * @param route         - {@link Route} entity of tracked process
     * @param plannedEvents - list of {@link PlannedEvent} of tracked process
     */
    public void updateEventStatusOfStopsForVp(@NotNull final Route route, @NotNull final List<PlannedEvent> plannedEvents) {
        final List<StopsForVp> stopsForVp = route.getStopsForVp();
        stopsForVp.forEach(stopForVp -> updateEventStatusOfStopForVp(plannedEvents, stopForVp));
    }

    private void updatePlannedSpots(final List<PlannedSpot> plannedSpots, final PlannedSpot nextPlannedSpot) {
        final Iterator<PlannedSpot> it = plannedSpots.iterator();
        while (it.hasNext()) {
            PlannedSpot plannedSpot = it.next();
            if (plannedSpot.getEventId().compareTo(nextPlannedSpot.getEventId()) != 0) {
                it.remove();
            } else {
                return;
            }
        }
    }

    private void updateEventStatusOfStopForVp(final List<PlannedEvent> plannedEvents, final StopsForVp stopForVp) {
        final String stopId = stopForVp.getStopId();
        final Optional<PlannedEvent> relatedPlannedEventOpt = plannedEvents.stream()
                .filter(plannedEvent -> isArrivalEventType(plannedEvent.getEventType()))
                .filter(plannedEvent -> plannedEvent.getEventMatchKey().equals(stopId))
                .findFirst();
        relatedPlannedEventOpt.ifPresent(relatedPlannedEvent -> {
            final EstimatedArrival estimatedArrival = stopForVp.getEstimatedArrival();
            if (nonNull(estimatedArrival)) {
                final String relatedPlannedEventStatusCode = relatedPlannedEvent.getEventStatusCode();
                estimatedArrival.setEventStatusCode(relatedPlannedEventStatusCode);
            }
        });
    }

    private boolean isArrivalEventType(final String eventType) {
        return ARRIVAL.getValue().equals(getEventTypeShortName(eventType));
    }
}
