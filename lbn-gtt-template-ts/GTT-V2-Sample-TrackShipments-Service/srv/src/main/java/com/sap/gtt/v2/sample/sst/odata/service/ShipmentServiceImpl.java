package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.removeParametersFromUrl;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.buildLocationAltKey;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;
import static java.lang.String.format;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.List;
import java.util.Optional;
import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.MultiValueMap;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class ShipmentServiceImpl implements ShipmentService {

    private static final String SHIPMENT_ENDPOINT = "/Shipment";
    private static final String ARRIVAL_LOCATION = "arrivalLocation";
    private static final String DEPARTURE_LOCATION = "departureLocation";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Autowired
    private LocationService locationService;

    @Override
    public Optional<Shipment> getById(@NotNull final String id) {
        final String uri = buildUriById(id, null);
        return getShipment(uri);
    }

    @Override
    public Optional<Shipment> getById(@NotNull final String id, @Nullable final MultiValueMap<String, String> params) {
        final String uri = buildUriById(id, params);
        return getShipment(uri);
    }

    @Override
    public Optional<Shipment> getByUri(@NotNull final String uri) {
        final String uriWithoutParameters = removeParametersFromUrl(uri, ARRIVAL_LOCATION, DEPARTURE_LOCATION);
        final Optional<Shipment> shipmentOpt = getShipment(uriWithoutParameters);
        shipmentOpt.ifPresent(shipment -> expandLocationsIfNeeded(uri, shipment));
        return shipmentOpt;
    }

    @Override
    public ODataResultList<Shipment> getAllByUri(@NotNull final String uri) {
        final String uriWithoutParameters = removeParametersFromUrl(uri, ARRIVAL_LOCATION, DEPARTURE_LOCATION);
        final ODataResultList<Shipment> shipmentResultList = gttCoreServiceClient.readEntitySet(uriWithoutParameters, Shipment.class);
        expandLocationsIfNeeded(uri, shipmentResultList.getResults());
        return shipmentResultList;
    }

    private Optional<Shipment> getShipment(final String uri) {
        final Shipment shipment = gttCoreServiceClient.readEntity(uri, Shipment.class);
        return Optional.ofNullable(shipment);
    }

    private void expandLocationsIfNeeded(final String uri, final Shipment shipment) {
        final String arrivalLocationAltKey = buildArrivalLocationAltKey(shipment);
        final String departureLocationAltKey = buildDepartureLocationAltKey(shipment);
        final Optional<Location> arrivalLocationOpt = getArrivalLocationIfNeeded(uri, arrivalLocationAltKey);
        final Optional<Location> departureLocationOpt = getDepartureLocationIfNeeded(uri, departureLocationAltKey);
        arrivalLocationOpt.ifPresent(shipment::setArrivalLocation);
        departureLocationOpt.ifPresent(shipment::setDepartureLocation);
    }

    private void expandLocationsIfNeeded(final String uri, final List<Shipment> shipments) {
        if (uri.contains(ARRIVAL_LOCATION) || uri.contains(DEPARTURE_LOCATION)) {
            final List<Location> locations = locationService.getAll();
            fillShipmentsWithLocations(shipments, locations);
        }
    }

    private void fillShipmentsWithLocations(final List<Shipment> shipments, final List<Location> locations) {
        shipments.forEach(shipment -> {
            final String arrivalLocationAltKey = buildArrivalLocationAltKey(shipment);
            final String departureLocationAltKey = buildDepartureLocationAltKey(shipment);
            final Optional<Location> arrivalLocationOpt = retrieveLocationByAltKey(arrivalLocationAltKey, locations);
            final Optional<Location> departureLocationOpt = retrieveLocationByAltKey(departureLocationAltKey, locations);
            arrivalLocationOpt.ifPresent(shipment::setArrivalLocation);
            departureLocationOpt.ifPresent(shipment::setDepartureLocation);
        });
    }

    private Optional<Location> getArrivalLocationIfNeeded(final String uri, final String arrivalLocationAltKey) {
        return uri.contains(ARRIVAL_LOCATION)
                ? locationService.getByAltKey(arrivalLocationAltKey)
                : Optional.empty();
    }

    private Optional<Location> getDepartureLocationIfNeeded(final String uri, final String departureLocationAltKey) {
        return uri.contains(DEPARTURE_LOCATION)
                ? locationService.getByAltKey(departureLocationAltKey)
                : Optional.empty();
    }

    private String buildArrivalLocationAltKey(final Shipment shipment) {
        final String scheme = shipment.getScheme();
        final String partyId = shipment.getPartyId();
        final String logicalSystem = shipment.getLogicalSystem();
        final String arrivalLocationTypeCode = shipment.getArrivalLocationTypeCode();
        final String arrivalLocationId = shipment.getArrivalLocationId();
        return buildLocationAltKey(scheme, partyId, logicalSystem, arrivalLocationTypeCode, arrivalLocationId);
    }

    private String buildDepartureLocationAltKey(final Shipment shipment) {
        final String scheme = shipment.getScheme();
        final String partyId = shipment.getPartyId();
        final String logicalSystem = shipment.getLogicalSystem();
        final String departureLocationTypeCode = shipment.getDepartureLocationTypeCode();
        final String departureLocationId = shipment.getDepartureLocationId();
        return buildLocationAltKey(scheme, partyId, logicalSystem, departureLocationTypeCode, departureLocationId);
    }

    private String buildUriById(final String id, final MultiValueMap<String, String> params) {
        return UriComponentsBuilder
                .fromPath(format("%s(guid'%s')", SHIPMENT_ENDPOINT, id))
                .queryParams(params)
                .build().encode().toUriString();
    }
}
