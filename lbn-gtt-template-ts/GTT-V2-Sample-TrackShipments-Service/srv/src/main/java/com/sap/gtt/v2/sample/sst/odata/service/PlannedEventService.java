package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link PlannedEventService} is a service which operates on {@link PlannedEvent} entities.
 *
 * @author Aliaksandr Miron
 */
public interface PlannedEventService {

    /**
     * Retrieves all {@link PlannedEvent} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of {@link PlannedEvent} entities.
     */
    List<PlannedEvent> getAllByTrackedProcessId(@NotNull final String trackedProcessId);
}
