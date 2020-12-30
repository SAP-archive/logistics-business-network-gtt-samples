package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline;

import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;

import javax.validation.constraints.NotNull;
import java.util.List;

public interface TimeTrackingService {
    public List<TimelineEvent> getByDeliveryItemId(@NotNull final String deliveryItemId) ;
}
