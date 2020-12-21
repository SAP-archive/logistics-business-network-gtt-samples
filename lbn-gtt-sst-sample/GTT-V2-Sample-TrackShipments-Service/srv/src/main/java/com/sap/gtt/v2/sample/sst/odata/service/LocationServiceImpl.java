package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class LocationServiceImpl implements LocationService {

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public Optional<Location> getByAltKey(@NotNull final String altKey) {
        return gttCoreServiceClient.getLocation(altKey);
    }

    @Override
    public List<Location> getAll() {
        return gttCoreServiceClient.getLocations();
    }

    @Override
    public ODataResultList<Location> getAll(final String uri) {
        return gttCoreServiceClient.getLocationsByUri(uri);
    }

    @Override
    public Location get(final String uri) {
        return gttCoreServiceClient.getLocationByUri(uri);
    }
}
