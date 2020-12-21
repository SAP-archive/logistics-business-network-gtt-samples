package com.sap.gtt.v2.sample.sst.rest.service;

import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import com.sap.gtt.v2.sample.sst.rest.model.converter.EventHistoryConverter;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class EventHistoryServiceImpl implements EventHistoryService {

    @Autowired
    private EventHistoryConverter eventHistoryConverter;

    @Override
    public Map<ProcessEventDirectory, List<EventHistory>> getEventHistoryForPlannedEvents(
            @NotNull final Map<ProcessEventDirectory, List<ProcessEventDirectory>> actualEventsByPlannedEvents) {
        return actualEventsByPlannedEvents.keySet().stream()
                .collect(toMap(
                        Function.identity(),
                        plannedEvent -> convertActualEventsToEventHistory(actualEventsByPlannedEvents.get(plannedEvent))));
    }

    private List<EventHistory> convertActualEventsToEventHistory(final List<ProcessEventDirectory> actualEvents) {
        return actualEvents.stream()
                .sorted(getProcessEventDirectoryComparator())
                .map(eventHistoryConverter::fromProcessEventDirectory)
                .collect(toList());
    }

    private Comparator<ProcessEventDirectory> getProcessEventDirectoryComparator() {
        return comparing((ProcessEventDirectory processEventDirectory) ->
                processEventDirectory.getEvent().getActualTechnicalTimestamp(), nullsFirst(Long::compareTo))
                .reversed();
    }
}
