package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline;

import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;

import javax.validation.constraints.NotNull;
import java.util.List;
public interface PlannedEventService {
    public List<PlannedEvent> getAllByDeliveryItemId(@NotNull final String deliveryItemId);
}
