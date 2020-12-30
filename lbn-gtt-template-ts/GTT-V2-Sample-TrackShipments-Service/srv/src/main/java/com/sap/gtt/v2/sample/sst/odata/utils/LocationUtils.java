package com.sap.gtt.v2.sample.sst.odata.utils;

import static java.lang.String.format;

import com.sap.gtt.v2.sample.sst.odata.model.Location;
import java.util.List;
import java.util.Optional;

/**
 * {@link LocationUtils} is util class which operates on {@link Location} entity.
 *
 * @author Aliaksandr Miron
 */
public class LocationUtils {

    private static final String LOCATION_PATH_SEGMENT = "Location";

    private LocationUtils() {
    }

    public static String buildLocationAltKey(
            final String scheme,
            final String partyId,
            final String logicalSystem,
            final String locationType,
            final String locationId) {
        return format("%s:%s:%s:%s:%s:%s",
                scheme, partyId, logicalSystem, LOCATION_PATH_SEGMENT, locationType, locationId);
    }

    public static Optional<Location> retrieveLocationByAltKey(
            final String locationAltKey,
            final List<Location> locations) {
        return locations.stream()
                .filter(location -> location.getLocationAltKey().equals(locationAltKey))
                .findFirst();
    }
}
