package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import com.sap.gtt.v2.sample.sst.rest.service.FreightUnitTimelineEventService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link FreightUnitTimelineEventController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.FreightUnit} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + FreightUnitTimelineEventController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class FreightUnitTimelineEventController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/freightUnits/timelineEvents";

    @Autowired
    private FreightUnitTimelineEventService freightUnitTimelineEventService;

    @GetMapping
    public List<TimelineEvent> getByFreightUnitId(@RequestParam final String freightUnitId) {
        return freightUnitTimelineEventService.getByTrackedProcessId(freightUnitId);
    }
}
