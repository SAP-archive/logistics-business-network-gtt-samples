package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sst.odata.service.PlannedEventService;
import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import java.util.List;
import java.util.Map;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class FreightUnitEventsByStatusService extends EventsByStatusAbstractService {

    @Autowired
    private PlannedEventService plannedEventService;

    @Override
    public List<EventsByStatus> getByTrackedProcessId(@NotNull final String trackedProcessId) {
        final List<PlannedEvent> plannedEvents = plannedEventService.getAllByTrackedProcessId(trackedProcessId);
        final Map<String, Integer> eventsByStatuses = collectEventsByStatusCodes();
        return getCalculatedEventsAmountByStatuses(eventsByStatuses, plannedEvents);
    }
}
