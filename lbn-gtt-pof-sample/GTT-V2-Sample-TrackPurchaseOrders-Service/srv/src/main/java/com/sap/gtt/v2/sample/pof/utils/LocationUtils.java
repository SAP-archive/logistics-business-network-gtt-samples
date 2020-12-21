package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.domain.Location;

import java.util.List;
import java.util.Optional;

public class LocationUtils {

    private LocationUtils() {
    }

    public static Optional<Location> retrieveLocationByAltKey(
            final String locationAltKey,
            final List<Location> locations) {
        return locations.stream()
                .filter(location -> location.getLocationAltKey().equals(locationAltKey))
                .findFirst();
    }
}