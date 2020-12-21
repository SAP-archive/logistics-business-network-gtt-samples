package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsLast;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.common.service.ProcessEventDirectoryService;
import com.sap.gtt.v2.sample.sst.common.utils.ProcessEventDirectoryUtils;
import com.sap.gtt.v2.sample.sst.common.validator.CoordinatesValidator;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.CurrentLocation;
import com.sap.gtt.v2.sample.sst.rest.model.converter.CurrentLocationConverter;
import java.util.Comparator;
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
public class CurrentLocationServiceImpl implements CurrentLocationService {

    @Autowired
    private ProcessEventDirectoryService processEventDirectoryService;

    @Autowired
    private CoordinatesValidator coordinatesValidator;

    @Autowired
    private CurrentLocationConverter currentLocationConverter;

    @Autowired
    private ActualSpotService actualSpotService;

    @Override
    public Optional<CurrentLocation> getByShipmentId(@NotNull final String shipmentId) {
        final List<ProcessEventDirectory> actualEvents = getActualEventsInWhitelist(shipmentId);
        final List<ActualSpot> actualSpots = actualSpotService.getAllAscending(actualEvents);
        return getFromActualSpots(actualSpots);
    }

    @Override
    public Optional<CurrentLocation> getFromActualSpots(@NotNull final List<ActualSpot> actualSpots) {
        return getMaxActualSpot(actualSpots)
                .filter(it -> coordinatesValidator.isValid(
                        it.getLongitude(),
                        it.getLatitude()))
                .map(currentLocationConverter::fromActualSpot);
    }

    private Optional<ActualSpot> getMaxActualSpot(final List<ActualSpot> actualSpots) {
        return actualSpots.stream().max(getActualSpotMaxComparator());
    }

    private Comparator<ActualSpot> getActualSpotMaxComparator() {
        return comparing(actualSpot -> actualSpot.getEvent().getActualBusinessTimestamp(), nullsLast(Long::compareTo));
    }

    private List<ProcessEventDirectory> getActualEventsInWhitelist(final String shipmentId) {
        final List<ProcessEventDirectory> processEventDirectories = processEventDirectoryService.getByShipmentId(shipmentId);
        return ProcessEventDirectoryUtils.filterByWhitelistForRoutes(processEventDirectories);
    }
}
