package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.Route;
import javax.validation.constraints.NotNull;

/**
 * {@link RouteService} is a service which operates on {@link Route} entities.
 *
 * @author Aliaksandr Miron
 */
public interface RouteService {

    /**
     * Retrieves {@link Route} by provided UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return {@link Route} entity
     */
    Route getByShipmentId(@NotNull final String shipmentId);
}
