package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.Route;
import com.sap.gtt.v2.sample.sst.rest.service.RouteService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link RouteController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + RouteController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class RouteController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/routes";

    @Autowired
    private RouteService routeService;

    @GetMapping
    public Route getByShipmentId(@RequestParam final String shipmentId) {
        return routeService.getByShipmentId(shipmentId);
    }
}
