package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import java.util.Optional;
import javax.annotation.Nullable;
import javax.validation.constraints.NotNull;
import org.springframework.util.MultiValueMap;

/**
 * {@link FreightUnitService} is a service which operates on {@link FreightUnit} entities.
 *
 * @author Aliaksandr Miron
 */
public interface FreightUnitService {

    /**
     * Retrieves {@link FreightUnit} entity by provided URI.
     *
     * @param uri - URI of request
     * @return {@link FreightUnit} entity wrapped in {@link Optional}
     */
    Optional<FreightUnit> getByUri(@NotNull final String uri);

    /**
     * Retrieves {@link FreightUnit} entities by provided URI.
     *
     * @param uri - URI of request
     * @return {@link FreightUnit} entities wrapped in {@link ODataResultList}
     */
    ODataResultList<FreightUnit> getAllByUri(@NotNull final String uri);

    /**
     * Retrieves {@link FreightUnit} entity by provided UUID and params.
     *
     * @param id     - UUID of {@link FreightUnit} entity
     * @param params - request params
     * @return {@link FreightUnit} entity wrapped in {@link Optional}
     */
    Optional<FreightUnit> getById(@NotNull final String id, @Nullable final MultiValueMap<String, String> params);
}
