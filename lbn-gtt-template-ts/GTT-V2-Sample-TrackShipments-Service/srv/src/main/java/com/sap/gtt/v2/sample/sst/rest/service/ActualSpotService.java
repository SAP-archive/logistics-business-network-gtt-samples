package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * {@link ActualSpotService} is a service which operates on {@link ActualSpot} entities.
 *
 * @author Aliaksandr Miron
 */
public interface ActualSpotService {

    /**
     * Retrieve list of {@link ActualSpot} from provided {@link ProcessEventDirectory} entities.
     *
     * @param processEventDirectories - provided list of {@link ProcessEventDirectory} entities
     * @return list of {@link ActualSpot} entities
     */
    List<ActualSpot> getAllAscending(@NotNull final List<ProcessEventDirectory> processEventDirectories);
}
