package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.PlannedSpot;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link PlannedSpotService} is a service which operates on {@link PlannedSpot} entities.
 *
 * @author Min Li
 */
public interface PlannedSpotService {

    /**
     * Operates on provided {@link PlannedEvent} entities and converts them to {@link PlannedSpot} entities.
     *
     * @param plannedEvents - provided {@link PlannedEvent} entities
     * @return list of {@link PlannedSpot} entities
     */
    List<PlannedSpot> getAll(@NotNull final List<PlannedEvent> plannedEvents);

    /**
     * Retrieves {@link PlannedSpot} entity.
     *
     * @param currentLocation - {@link CurrentLocation} entity
     * @param plannedSpots    - list of {@link PlannedSpot} entities
     * @param plannedEvents   - list of {@link PlannedEvent} entities
     * @return {@link PlannedSpot} entity
     */
    PlannedSpot findNextPlannedSpot(
            final CurrentLocation currentLocation,
            final List<PlannedSpot> plannedSpots,
            final List<PlannedEvent> plannedEvents);
}
