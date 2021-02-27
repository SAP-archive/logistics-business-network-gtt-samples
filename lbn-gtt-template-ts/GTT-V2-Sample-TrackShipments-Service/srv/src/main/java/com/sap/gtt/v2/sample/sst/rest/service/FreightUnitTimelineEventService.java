package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.rest.helper.TimelineEventHelper;
import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import java.util.List;
import java.util.stream.Stream;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class FreightUnitTimelineEventService extends TimelineEventAbstractService {

    @Autowired
    private TimelineEventHelper timelineEventHelper;

    @Override
    public List<TimelineEvent> getByTrackedProcessId(@NotNull final String trackedProcessId) {
        final List<TimelineEvent> plannedTimelineEvents = getPlannedTimelineEvents(trackedProcessId);
        final List<TimelineEvent> actualTimelineEvents = getActualTimelineEvents(trackedProcessId);
        final List<TimelineEvent> timelineEvents =
                Stream.concat(plannedTimelineEvents.stream(), actualTimelineEvents.stream()).collect(toList());
        timelineEventHelper.fillETAData(timelineEvents, trackedProcessId);
        return timelineEvents;
    }
}
