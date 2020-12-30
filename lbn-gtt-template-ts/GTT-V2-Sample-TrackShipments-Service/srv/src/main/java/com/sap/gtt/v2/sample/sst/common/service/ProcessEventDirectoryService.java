package com.sap.gtt.v2.sample.sst.common.service;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.validation.constraints.NotNull;

/**
 * {@link ProcessEventDirectoryService} is a service which operates on {@link ProcessEventDirectory} entities.
 *
 * @author Aliaksandr Miron
 */
public interface ProcessEventDirectoryService {

    /**
     * Retrieves {@link ProcessEventDirectory} entities by provided
     * {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} UUID.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return list of {@link ProcessEventDirectory} entities
     */
    List<ProcessEventDirectory> getByShipmentId(@NotNull final String shipmentId);

    /**
     * Retrieves {@link ProcessEventDirectory} entities by provided UUIDs.
     *
     * @param ids - UUIDs of {@link ProcessEventDirectory} entities
     * @return list of {@link ProcessEventDirectory} entities
     */
    List<ProcessEventDirectory> getByIds(@NotNull final List<UUID> ids);

    /**
     * Processes provided {@link ProcessEventDirectory} list to group actual events by theirs planned events.
     *
     * @param events - list of {@link ProcessEventDirectory} entities
     * @return {@link Map} of actual events in {@link ProcessEventDirectory} by theirs
     * planned events in {@link ProcessEventDirectory}
     */
    Map<ProcessEventDirectory, List<ProcessEventDirectory>> getActualEventsByPlannedEvents(
            @NotNull final List<ProcessEventDirectory> events);

    /**
     * Processes provided {@link ProcessEventDirectory} entities and retrieves events without planned event.
     *
     * @param processEventDirectories - list of {@link ProcessEventDirectory} entities
     * @return list of {@link ProcessEventDirectory} without planned event
     */
    List<ProcessEventDirectory> getWithoutPlannedEvent(@NotNull final List<ProcessEventDirectory> processEventDirectories);
}
