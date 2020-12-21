package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.Optional;
import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;
import org.springframework.util.MultiValueMap;

/**
 * {@link ShipmentService} is a service which operates on {@link Shipment} entities.
 *
 * @author Aliaksandr Miron
 */
public interface ShipmentService {

    /**
     * * Retrieves {@link Shipment} entity by provided UUID.
     *
     * @param id - UUID of {@link Shipment} entity
     * @return {@link Shipment} entity wrapped in {@link Optional}
     */
    Optional<Shipment> getById(@NotNull final String id);

    /**
     * Retrieves {@link Shipment} entity by provided UUID and params.
     *
     * @param id     - UUID of {@link Shipment} entity
     * @param params - request params
     * @return {@link Shipment} entity wrapped in {@link Optional}
     */
    Optional<Shipment> getById(@NotNull final String id, @Nullable final MultiValueMap<String, String> params);

    /**
     * Retrieves {@link Shipment} entity by provided URI.
     *
     * @param uri - URI of request
     * @return {@link Shipment} entity wrapped in {@link Optional}
     */
    Optional<Shipment> getByUri(@NotNull final String uri);

    /**
     * Retrieves {@link Shipment} entities by provided URI.
     *
     * @param uri - URI of request
     * @return {@link Shipment} entities wrapped in {@link ODataResultList}
     */
    ODataResultList<Shipment> getAllByUri(@NotNull final String uri);
}
