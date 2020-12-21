package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link EventsByStatusService} is a service which operates on {@link EventsByStatus} entities.
 *
 * @author Aliaksandr Miron
 */
public interface EventsByStatusService {

    /**
     * Returns {@link EventsByStatus} entities by provided UUID of
     * {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return list of {@link EventsByStatus} entities
     */
    List<EventsByStatus> getByShipmentId(@NotNull final String shipmentId);
}
