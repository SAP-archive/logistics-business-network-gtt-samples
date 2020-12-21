package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.NotNull;

/**
 * {@link EventHistoryService} is a service which operates on {@link EventHistory} entities.
 *
 * @author Aliaksandr Miron
 */
public interface EventHistoryService {

    /**
     * Retrieves planned events with theirs {@link EventHistory} entities.
     *
     * @param actualEventsByPlannedEvents - {@link ProcessEventDirectory} entities grouped by planned events.
     * @return planned events with theirs {@link EventHistory} entities.
     */
    Map<ProcessEventDirectory, List<EventHistory>> getEventHistoryForPlannedEvents(
            @NotNull final Map<ProcessEventDirectory, List<ProcessEventDirectory>> actualEventsByPlannedEvents);
}
