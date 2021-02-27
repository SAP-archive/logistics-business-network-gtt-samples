package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.Route;
import com.sap.gtt.v2.sample.sst.rest.service.ShipmentRouteService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link ShipmentRouteController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + ShipmentRouteController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class ShipmentRouteController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/shipments/routes";

    @Autowired
    private ShipmentRouteService shipmentRouteService;

    @GetMapping
    public Route getByShipmentId(@RequestParam final String shipmentId) {
        return shipmentRouteService.getByTrackedProcessId(shipmentId);
    }
}
