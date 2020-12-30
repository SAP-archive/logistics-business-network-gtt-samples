package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline;

import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Set;


public interface LocationService {
    public List<Location> getLocationsWithLocationAltKeys(@NotNull final Set<String> locationAltKeys);

    void fillLocations(@NotNull List<TimelineEvent> timelineEvents);
}
