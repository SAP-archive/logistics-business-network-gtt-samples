package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.buildLocationAltKey;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;

import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.odata.service.LocationService;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link StopsForVpConverter} is a converter which converts provided entities to {@link StopsForVp}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class StopsForVpConverter {

    @Autowired
    private LocationService locationService;

    /**
     * Converts provided {@link Shipment} entity to {@link StopsForVp} entities.
     *
     * @param shipment - {@link Shipment} entity to be converted
     * @return list of {@link StopsForVp} entities
     */
    public List<StopsForVp> fromShipment(@NotNull final Shipment shipment) {
        final String scheme = shipment.getScheme();
        final String partyId = shipment.getPartyId();
        final String logicalSystem = shipment.getLogicalSystem();
        final List<StopsForVp> stopsForVp = shipment.getStopsForVp();
        final List<Location> locations = locationService.getAll();
        stopsForVp.forEach(stopForVp -> {
            final String locationTypeCode = stopForVp.getLocationTypeCode();
            final String locationId = stopForVp.getLocationId();
            final String locationAltKey = buildLocationAltKey(scheme, partyId, logicalSystem, locationTypeCode, locationId);
            final Optional<Location> locationOpt = retrieveLocationByAltKey(locationAltKey, locations);
            locationOpt.ifPresent(stopForVp::setLocation);
        });
        return stopsForVp;
    }
}
