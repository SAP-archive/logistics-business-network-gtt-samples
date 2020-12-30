package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Comparator.comparing;

import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.EstimatedArrival;
import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import com.sap.gtt.v2.sample.sst.rest.model.converter.NextStopConverter;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class NextStopServiceImpl implements NextStopService {

    @Autowired
    private CurrentLocationService currentLocationService;

    @Autowired
    private NextStopConverter nextStopConverter;

    @Override
    public Optional<NextStop> getByShipmentId(@NotNull final String shipmentId) {
        final Optional<CurrentLocation> currentLocationOpt = currentLocationService.getByShipmentId(shipmentId);
        return currentLocationOpt.flatMap(currentLocation -> convertToNextStop(currentLocation, shipmentId));
    }

    private Optional<NextStop> convertToNextStop(final CurrentLocation currentLocation, final String shipmentId) {
        final List<EstimatedArrival> estimatedArrivals = currentLocation.getEstimatedArrival();
        return estimatedArrivals.isEmpty()
                ? Optional.empty()
                : getSmallestEstimatedArrival(estimatedArrivals).map(
                estimatedArrival -> nextStopConverter.fromEstimatedArrival(estimatedArrival, shipmentId));
    }

    private Optional<EstimatedArrival> getSmallestEstimatedArrival(final List<EstimatedArrival> estimatedArrivals) {
        return estimatedArrivals.stream().min(comparing(EstimatedArrival::getStopId));
    }
}
