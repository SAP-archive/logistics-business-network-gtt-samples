package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.NextStop;
import com.sap.gtt.v2.sample.sst.rest.service.NextStopService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link NextStopController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + NextStopController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class NextStopController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/nextStop";

    @Autowired
    private NextStopService nextStopService;

    @GetMapping
    public NextStop getByShipmentId(@RequestParam final String shipmentId) {
        return nextStopService.getByShipmentId(shipmentId).orElse(null);
    }
}
