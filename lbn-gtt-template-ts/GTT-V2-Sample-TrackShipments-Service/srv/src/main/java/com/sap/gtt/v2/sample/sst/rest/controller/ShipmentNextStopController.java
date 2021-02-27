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
 * {@link ShipmentNextStopController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + ShipmentNextStopController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class ShipmentNextStopController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/shipments/nextStop";

    @Autowired
    private NextStopService nextStopService;

    @GetMapping
    public NextStop getByShipmentId(@RequestParam final String shipmentId) {
        return nextStopService.getByTrackedProcessId(shipmentId).orElse(null);
    }
}
