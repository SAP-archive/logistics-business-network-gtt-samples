package com.sap.gtt.v2.sample.sst.common.utils;

import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType.DELAY;
import static com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType.LOCATION_UPDATE;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;
import static java.util.Comparator.comparing;
import static java.util.Comparator.nullsFirst;
import static java.util.Comparator.nullsLast;
import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.common.constant.ShipmentEventType;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * {@link ProcessEventDirectoryUtils} is an util class for {@link ProcessEventDirectory} entity.
 *
 * @author Aliaksandr Miron
 */
public class ProcessEventDirectoryUtils {

    private ProcessEventDirectoryUtils() {
    }

    public static List<ProcessEventDirectory> filterByWhitelistForTimelineEvents(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(it -> isSuitableEventTypeForTimelineEvent(it.getEvent().getEventType()))
                .filter(it -> ShipmentEventType.containsEventType(it.getEvent().getEventType()))
                .collect(toList());
    }

    public static List<ProcessEventDirectory> filterByWhitelistForRoutes(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(it -> ShipmentEventType.containsEventType(it.getEvent().getEventType()))
                .collect(toList());
    }

    public static Optional<ProcessEventDirectory> getMaxByActualBusinessDateTime(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream().max(getMaxComparator());
    }

    public static List<ProcessEventDirectory> sortByActualBusinessDateTimeAscending(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .sorted(getAscendingComparator())
                .collect(toList());
    }

    public static List<ProcessEventDirectory> sortByActualBusinessDateTimeDescending(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .sorted(getDescendingComparator())
                .collect(toList());
    }

    public static Comparator<ProcessEventDirectory> getMaxComparator() {
        return comparing(processEventDirectory ->
                processEventDirectory.getEvent().getActualBusinessTimestamp(), nullsLast(Long::compareTo));
    }

    public static Comparator<ProcessEventDirectory> getDescendingComparator() {
        return comparing((ProcessEventDirectory processEventDirectory) ->
                processEventDirectory.getEvent().getActualBusinessTimestamp(), nullsFirst(Long::compareTo))
                .reversed();
    }

    public static Comparator<ProcessEventDirectory> getAscendingComparator() {
        return comparing((ProcessEventDirectory processEventDirectory) ->
                processEventDirectory.getEvent().getActualBusinessTimestamp(), nullsLast(Long::compareTo));
    }

    private static boolean isSuitableEventTypeForTimelineEvent(final String eventType) {
        final String eventTypeShortName = getEventTypeShortName(eventType);
        return !DELAY.getValue().equals(eventTypeShortName) && !LOCATION_UPDATE.getValue().equals(eventTypeShortName);
    }
}
