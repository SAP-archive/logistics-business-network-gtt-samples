package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;

/**
 * {@link LocationService} is a service which operates on {@link Location} entities.
 *
 * @author Aliaksandr Miron
 */
public interface LocationService {

    /**
     * Retrieves {@link Location} entity by provided key.
     *
     * @param altKey - key value of {@link Location} entity
     * @return {@link Location} entity wrapped in {@link Optional}
     */
    Optional<Location> getByAltKey(@NotNull final String altKey);

    /**
     * Retrieves all {@link Location} entities.
     *
     * @return all {@link Location} entities.
     */
    List<Location> getAll();

    /**
     * Retrieves {@link Location} entities by provided URI.
     *
     * @param uri - URI of the request
     * @return {@link Location} entities wrapped in {@link ODataResultList}
     */
    ODataResultList<Location> getAll(final String uri);

    /**
     * Retrieves {@link Location} entity by provided URI.
     *
     * @param uri - URI of the request
     * @return {@link Location} entity
     */
    Location get(final String uri);
}
