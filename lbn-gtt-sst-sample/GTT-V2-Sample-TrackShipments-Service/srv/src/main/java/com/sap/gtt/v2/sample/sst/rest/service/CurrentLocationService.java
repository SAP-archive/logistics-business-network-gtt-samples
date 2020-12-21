package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;

/**
 * {@link CurrentLocationService} is a service which operates on {@link CurrentLocation} entities.
 *
 * @author Aliaksandr Miron
 */
public interface CurrentLocationService {

    /**
     * Retrieve {@link CurrentLocation} by provided UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment}.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return {@link CurrentLocation} entity wrapped in {@link Optional}
     */
    Optional<CurrentLocation> getByShipmentId(@NotNull final String shipmentId);

    /**
     * Retrieve {@link CurrentLocation} by provided {@link ActualSpot} entities.
     *
     * @param actualSpots - list of {@link ActualSpot} entities
     * @return {@link CurrentLocation} entity wrapped in {@link Optional}
     */
    Optional<CurrentLocation> getFromActualSpots(@NotNull final List<ActualSpot> actualSpots);
}
