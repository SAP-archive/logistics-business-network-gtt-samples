package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.removeParametersFromUrl;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.buildLocationAltKey;
import static com.sap.gtt.v2.sample.sst.odata.utils.LocationUtils.retrieveLocationByAltKey;
import static java.lang.String.format;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
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
public class FreightUnitServiceImpl implements FreightUnitService {

    private static final String FREIGHT_UNIT_ENDPOINT = "/FreightUnit";
    private static final String ARRIVAL_LOCATION = "arrivalLocation";
    private static final String DEPARTURE_LOCATION = "departureLocation";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Autowired
    private LocationService locationService;

    @Override
    public Optional<FreightUnit> getByUri(@NotNull final String uri) {
        final String uriWithoutParameters = removeParametersFromUrl(uri, ARRIVAL_LOCATION, DEPARTURE_LOCATION);
        final Optional<FreightUnit> freightUnitOpt = getFreightUnit(uriWithoutParameters);
        freightUnitOpt.ifPresent(freightUnit -> expandLocationsIfNeeded(uri, freightUnit));
        return freightUnitOpt;
    }

    @Override
    public ODataResultList<FreightUnit> getAllByUri(@NotNull final String uri) {
        final String uriWithoutParameters = removeParametersFromUrl(uri, ARRIVAL_LOCATION, DEPARTURE_LOCATION);
        final ODataResultList<FreightUnit> freightUnitResultList =
                gttCoreServiceClient.readEntitySet(uriWithoutParameters, FreightUnit.class);
        expandLocationsIfNeeded(uri, freightUnitResultList.getResults());
        return freightUnitResultList;
    }

    @Override
    public Optional<FreightUnit> getById(@NotNull final String id, @Nullable final MultiValueMap<String, String> params) {
        final String uri = buildUriById(id, params);
        return getFreightUnit(uri);
    }

    private Optional<FreightUnit> getFreightUnit(final String uri) {
        final FreightUnit freightUnit = gttCoreServiceClient.readEntity(uri, FreightUnit.class);
        return Optional.ofNullable(freightUnit);
    }

    private void expandLocationsIfNeeded(final String uri, final FreightUnit freightUnit) {
        final String arrivalLocationAltKey = buildArrivalLocationAltKey(freightUnit);
        final String departureLocationAltKey = buildDepartureLocationAltKey(freightUnit);
        final Optional<Location> arrivalLocationOpt = getArrivalLocationIfNeeded(uri, arrivalLocationAltKey);
        final Optional<Location> departureLocationOpt = getDepartureLocationIfNeeded(uri, departureLocationAltKey);
        arrivalLocationOpt.ifPresent(freightUnit::setArrivalLocation);
        departureLocationOpt.ifPresent(freightUnit::setDepartureLocation);
    }

    private void expandLocationsIfNeeded(final String uri, final List<FreightUnit> freightUnits) {
        if (uri.contains(ARRIVAL_LOCATION) || uri.contains(DEPARTURE_LOCATION)) {
            final List<Location> locations = locationService.getAll();
            fillWithLocations(freightUnits, locations);
        }
    }

    private void fillWithLocations(final List<FreightUnit> freightUnits, final List<Location> locations) {
        freightUnits.forEach(freightUnit -> {
            final String arrivalLocationAltKey = buildArrivalLocationAltKey(freightUnit);
            final String departureLocationAltKey = buildDepartureLocationAltKey(freightUnit);
            final Optional<Location> arrivalLocationOpt = retrieveLocationByAltKey(arrivalLocationAltKey, locations);
            final Optional<Location> departureLocationOpt = retrieveLocationByAltKey(departureLocationAltKey, locations);
            arrivalLocationOpt.ifPresent(freightUnit::setArrivalLocation);
            departureLocationOpt.ifPresent(freightUnit::setDepartureLocation);
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

    private String buildArrivalLocationAltKey(final FreightUnit freightUnit) {
        final String scheme = freightUnit.getScheme();
        final String partyId = freightUnit.getPartyId();
        final String logicalSystem = freightUnit.getLogicalSystem();
        final String arrivalLocationTypeCode = freightUnit.getArrivalLocationTypeCode();
        final String arrivalLocationId = freightUnit.getArrivalLocationId();
        return buildLocationAltKey(scheme, partyId, logicalSystem, arrivalLocationTypeCode, arrivalLocationId);
    }

    private String buildDepartureLocationAltKey(final FreightUnit freightUnit) {
        final String scheme = freightUnit.getScheme();
        final String partyId = freightUnit.getPartyId();
        final String logicalSystem = freightUnit.getLogicalSystem();
        final String departureLocationTypeCode = freightUnit.getDepartureLocationTypeCode();
        final String departureLocationId = freightUnit.getDepartureLocationId();
        return buildLocationAltKey(scheme, partyId, logicalSystem, departureLocationTypeCode, departureLocationId);
    }

    private String buildUriById(final String id, final MultiValueMap<String, String> params) {
        return UriComponentsBuilder
                .fromPath(format("%s(guid'%s')", FREIGHT_UNIT_ENDPOINT, id))
                .queryParams(params)
                .build().encode().toUriString();
    }
}
