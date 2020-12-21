package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.model.EventStatus;
import java.util.List;

/**
 * {@link EventStatusService} is a service which operates on {@link EventStatus} entities.
 *
 * @author Aliaksandr Miron
 */
public interface EventStatusService {

    /**
     * Retrieves all {@link EventStatus} entities.
     *
     * @return all {@link EventStatus} entities.
     */
    List<EventStatus> getAll();
}
