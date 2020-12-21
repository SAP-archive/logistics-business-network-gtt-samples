package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.AdmissibleUnplannedEvent;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link ModelService} is a service which operates on {@link AdmissibleUnplannedEvent} entities.
 *
 * @author Min Li
 */
public interface ModelService {

    /**
     * Retrieves {@link AdmissibleUnplannedEvent} entities by provided tracked process name.
     *
     * @param trackedProcess - name of tracked process
     * @return list of {@link AdmissibleUnplannedEvent} entities
     */
    List<AdmissibleUnplannedEvent> getUnplannedEventTypesOfTp(@NotNull final String trackedProcess);
}
