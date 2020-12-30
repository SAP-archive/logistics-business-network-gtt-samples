package com.sap.gtt.v2.sample.sst.odata.service;

import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link StopsForVpService} is a service which operates on {@link StopsForVp} entities.
 *
 * @author Aliaksandr Miron
 */
public interface StopsForVpService {

    /**
     * Retrieves all {@link StopsForVp} entities by provided UUID of
     * {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
     * @return list of {@link StopsForVp} entities
     */
    List<StopsForVp> getAll(@NotNull final String shipmentId);
}
