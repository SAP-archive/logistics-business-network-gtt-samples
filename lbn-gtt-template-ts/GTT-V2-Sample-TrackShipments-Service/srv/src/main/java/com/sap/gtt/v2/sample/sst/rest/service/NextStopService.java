package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import java.util.Optional;
import javax.validation.constraints.NotNull;

/**
 * {@link NextStopService} is a service which operates on {@link NextStop} entities.
 *
 * @author Aliaksandr Miron
 */
public interface NextStopService {

    /**
     * Retrieves {@link NextStop} by provided UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return {@link NextStop} entity wrapped in {@link Optional}
     */
    Optional<NextStop> getByShipmentId(@NotNull final String shipmentId);
}
