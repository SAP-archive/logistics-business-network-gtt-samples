package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.impl;

import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.LocationService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import static com.sap.gtt.v2.sample.pof.utils.LocationUtils.retrieveLocationByAltKey;
import static java.util.Objects.nonNull;
import static java.util.stream.Collectors.toSet;


@Service
public class LocationServiceImpl implements LocationService {

    public static final String SEPARATOR = ":";
    private final GTTCoreServiceClient gttCoreServiceClient;

    public LocationServiceImpl(final GTTCoreServiceClient gttCoreServiceClient) {
        this.gttCoreServiceClient = gttCoreServiceClient;
    }

    @Override
    public List<Location> getLocationsWithLocationAltKeys(@NotNull Set<String> locationAltKeys) {
        return gttCoreServiceClient.getLocations(locationAltKeys);
    }

    @Override
    public void fillLocations(List<TimelineEvent> timelineEvents) {
        Set<String> locationAltKeys = timelineEvents.stream()
                .filter(timelineEvent -> nonNull(timelineEvent.getLocationAltKey()))
                .map(TimelineEvent::getLocationAltKey)
                .collect(toSet());
        List<Location> locations = getLocationsWithLocationAltKeys(locationAltKeys);
        timelineEvents.stream()
                .filter(timelineEvent -> nonNull(timelineEvent.getLocationAltKey()))
                .forEach(timelineEvent -> {
                    String locationAltKey = timelineEvent.getLocationAltKey();
                    Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
                    if (locationOpt.isPresent()) {
                       timelineEvent.setLocation(locationOpt.get());
                    } else {
                        timelineEvent.setLocationId(StringUtils.substringAfterLast(locationAltKey, SEPARATOR));
                    }
                });
    }
}
