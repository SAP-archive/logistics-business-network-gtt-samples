package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsLast;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.odata.service.ShipmentStopsForVpService;
import com.sap.gtt.v2.sample.sst.rest.helper.RouteHelper;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import com.sap.gtt.v2.sample.sst.rest.model.Route;
import java.util.Comparator;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class ShipmentRouteService extends RouteAbstractService {

    @Autowired
    private PlannedEventService plannedEventService;

    @Autowired
    private ShipmentStopsForVpService shipmentStopsForVpService;

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private RouteHelper routeHelper;

    @Override
    public Route getByTrackedProcessId(@NotNull final String shipmentId) {
        final Route route = new Route();
        final List<PlannedEvent> plannedEvents = getPlannedEvents(shipmentId);
        final List<StopsForVp> stopsForVp = shipmentStopsForVpService.getAll(shipmentId);
        final List<ActualSpot> actualSpots = getActualSpots(shipmentId);
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

    private List<ActualSpot> getActualSpots(final String shipmentId) {
        final List<ProcessEventDirectory> actualEvents = getActualEvents(shipmentId);
        return actualSpotHelper.getAllAscending(actualEvents);
    }

    private List<ProcessEventDirectory> getActualEvents(final String trackedProcessId) {
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByTrackedProcessId(trackedProcessId);
        return ProcessEventDirectoryUtils.filterByWhitelistForRoutes(processEventDirectories);
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
}
