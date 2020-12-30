package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link TimelineEventService} is a service which operates on {@link TimelineEvent} entities.
 *
 * @author Aliaksandr Miron
 */
public interface TimelineEventService {

    /**
     * Retrieves {@link TimelineEvent} entities by provided UUID of
     * {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return list of {@link TimelineEvent} entities
     */
    List<TimelineEvent> getByShipmentId(@NotNull final String shipmentId);
}
