package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getDateTimeString;
import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getEventTypeShortName;

import com.sap.gtt.v2.sample.sst.common.model.Event;
import com.sap.gtt.v2.sample.sst.common.model.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sst.rest.model.EventHistory;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link EventHistoryConverter} is a converter which converts provided entities to {@link EventHistory}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class EventHistoryConverter {

    /**
     * Converts provided {@link ProcessEventDirectory} entity to {@link EventHistory} entity.
     *
     * @param processEventDirectory - {@link ProcessEventDirectory} entity to be converted
     * @return {@link EventHistory} entity
     */
    public EventHistory fromProcessEventDirectory(@NotNull final ProcessEventDirectory processEventDirectory) {
        final EventHistory eventHistory = new EventHistory();
        final Event event = processEventDirectory.getEvent();
        eventHistory.setId(processEventDirectory.getEventId());
        eventHistory.setAltKey(event.getAltKey());
        eventHistory.setActualBusinessTimestamp(getDateTimeString(event.getActualBusinessTimestamp()));
        eventHistory.setActualTechnicalTimestamp(getDateTimeString(event.getActualTechnicalTimestamp()));
        eventHistory.setActualBusinessTimeZone(event.getActualBusinessTimeZone());
        eventHistory.setEventType(getEventTypeShortName(event.getEventType()));
        eventHistory.setLongitude(event.getLongitude());
        eventHistory.setLatitude(event.getLatitude());
        eventHistory.setLocationAltKey(event.getLocationAltKey());
        eventHistory.setReportedBy(event.getReportedBy());
        eventHistory.setSenderPartyId(event.getSenderPartyId());
        eventHistory.setEventMatchKey(event.getEventMatchKey());
        return eventHistory;
    }
}
