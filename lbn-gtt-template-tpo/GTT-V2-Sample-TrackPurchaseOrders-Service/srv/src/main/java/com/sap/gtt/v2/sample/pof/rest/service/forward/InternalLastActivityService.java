package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.EventEx;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.UUID;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Service
public class InternalLastActivityService {

    private static final String LAST_CORRELATED_EVENT_ID = "lastCorrelatedEventId";
    private static final String ID = "id";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    public void updateLastActivityOfDeliveryItem(JsonObject trackedProcess, InboundDeliveryItem deliveryItem) {
        String lastCorrelatedEventId = trackedProcess.get(LAST_CORRELATED_EVENT_ID).getAsString();
        String tpId = trackedProcess.get(ID).getAsString();
        if (StringUtils.isEmpty(lastCorrelatedEventId) || StringUtils.isEmpty(tpId)) {
            return;
        }

        String query = UriComponentsBuilder.fromUriString("/Event(guid'" + lastCorrelatedEventId + "')")
                .build().encode().toUriString();

        Event event = gttCoreServiceClient.readEntity(query, Event.class);
        String eventType = event.getEventType().substring(event.getEventType().lastIndexOf(".") + 1);
        query = UriComponentsBuilder.fromUriString("/" + eventType + "(guid'" + lastCorrelatedEventId + "')")
                .queryParam(EXPAND, "eventProcesses/plannedEvent")
                .build().encode().toUriString();

        EventEx actualEvent = gttCoreServiceClient.readEntity(query, EventEx.class);
        PlannedEvent plannedEvent = null;
        for (ProcessEventDirectory ped : actualEvent.getEventProcesses()) {
            if (UUID.fromString(tpId).equals(ped.getProcessId())) {
                plannedEvent = ped.getPlannedEvent();
                break;
            }
        }

        String lastEventName, lastVPLocationTypeCode, lastLocationAltKey = null;
        lastVPLocationTypeCode = actualEvent.getLocationTypeCode();
        if (plannedEvent != null) {
            lastLocationAltKey = plannedEvent.getLocationAltKey();
        } else if (isNotBlank(actualEvent.getLocationAltKey())) {
            lastLocationAltKey = actualEvent.getLocationAltKey();
        } else {
            lastLocationAltKey = EMPTY;
        }

        String eventName = actualEvent.getEventType();
        lastEventName = eventName.substring(eventName.lastIndexOf(".") + 1);

        deliveryItem.setLastEventName(lastEventName);
        deliveryItem.setLastLocationAltKey(lastLocationAltKey);
        deliveryItem.setLastVPLocationTypeCode(lastVPLocationTypeCode);
    }
}
