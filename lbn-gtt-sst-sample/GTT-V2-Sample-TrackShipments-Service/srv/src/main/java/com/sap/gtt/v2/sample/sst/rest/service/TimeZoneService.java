package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.TimeZoneList;

/**
 * {@link TimeZoneService} is a service which operates on {@link TimeZoneList} entities.
 *
 * @author Min Li
 */
public interface TimeZoneService {

    /**
     * Retrieves all {@link TimeZoneList} entities.
     *
     * @return {@link TimeZoneList} entity
     */
    TimeZoneList getAllTimeZones();
}
