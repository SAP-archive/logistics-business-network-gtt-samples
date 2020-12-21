package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import com.sap.gtt.v2.sample.sst.rest.service.EventsByStatusService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link EventsByStatusController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + EventsByStatusController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class EventsByStatusController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/eventsByStatus";

    @Autowired
    private EventsByStatusService eventsByStatusService;

    @GetMapping
    public List<EventsByStatus> getByShipmentId(@RequestParam final String shipmentId) {
        return eventsByStatusService.getByShipmentId(shipmentId);
    }
}
