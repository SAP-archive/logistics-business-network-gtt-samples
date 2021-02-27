package com.sap.gtt.v2.sample.sst.odata.service;

import static java.util.Collections.emptyList;

import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * {@link StopsForVpAbstractService} is an abstract service which operates on {@link StopsForVp} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Service
public abstract class StopsForVpAbstractService<T> {

    protected static final String STOPS_FOR_VP_FIELD = "stopsForVP";

    /**
     * Retrieves all {@link StopsForVp} entities by provided UUID of tracked process.
     *
     * @param trackedProcessId - UUID of tracked process
     * @return list of {@link StopsForVp} entities
     */
    public List<StopsForVp> getAll(@NotNull final String trackedProcessId) {
        final Optional<T> trackedProcessOpt = getTrackedProcess(trackedProcessId);
        return trackedProcessOpt.isPresent()
                ? convertFromTrackedProcess(trackedProcessOpt.get())
                : emptyList();
    }

    protected abstract Optional<T> getTrackedProcess(@NotNull final String trackedProcessId);

    protected abstract List<StopsForVp> convertFromTrackedProcess(@NotNull final T trackedProcess);
}
