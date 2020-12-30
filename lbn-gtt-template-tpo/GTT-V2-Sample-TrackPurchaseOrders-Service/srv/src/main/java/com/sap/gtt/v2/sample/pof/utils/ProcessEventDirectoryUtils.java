package com.sap.gtt.v2.sample.pof.utils;

import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

import static java.util.Objects.isNull;
import static java.util.stream.Collectors.toList;

public class ProcessEventDirectoryUtils {

    public static final char DOT = '.';
    public static final String GTT = "GTT";
    public static final String UNPLANNED = "UNPLANNED";
    public static final String LOCATION_UPDATE = "LocationUpdate";

    private ProcessEventDirectoryUtils() {
    }

    public static List<ProcessEventDirectory> filterForUnplannedEvent(
            final List<ProcessEventDirectory> processEventDirectories) {
        return processEventDirectories.stream()
                .filter(processEventDirectory -> isNull(processEventDirectory.getPlannedEventId()))
                .filter(processEventDirectory -> isWhiteListCorrelationTypeCode(processEventDirectory.getCorrelationTypeCode()))
                .filter(processEventDirectory -> !isBlackListEventTypeForTimelineEvent(processEventDirectory.getEvent().getEventType()))
                .collect(toList());
    }

    private static boolean isBlackListEventTypeForTimelineEvent(String eventType) {
        return StringUtils.contains(eventType.substring(eventType.lastIndexOf(DOT)),GTT)||StringUtils.contains(eventType,LOCATION_UPDATE);
    }

    private static boolean isWhiteListCorrelationTypeCode(String correlationTypeCode) {
        return StringUtils.equals(UNPLANNED,correlationTypeCode);
    }
}
