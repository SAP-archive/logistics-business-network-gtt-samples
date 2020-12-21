package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.TimelineEvent;
import com.sap.gtt.v2.sample.sst.rest.service.TimelineEventService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link TimelineEventController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + TimelineEventController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class TimelineEventController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/timelineEvents";

    @Autowired
    private TimelineEventService timelineEventService;

    @GetMapping
    public List<TimelineEvent> getByShipmentId(@RequestParam final String shipmentId) {
        return timelineEventService.getByShipmentId(shipmentId);
    }
}
