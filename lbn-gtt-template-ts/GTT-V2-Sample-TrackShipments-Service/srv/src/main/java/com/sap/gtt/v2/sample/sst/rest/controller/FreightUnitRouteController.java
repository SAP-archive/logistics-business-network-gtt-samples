package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.Route;
import com.sap.gtt.v2.sample.sst.rest.service.FreightUnitRouteService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link FreightUnitRouteController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.FreightUnit} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + FreightUnitRouteController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class FreightUnitRouteController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/freightUnits/routes";

    @Autowired
    private FreightUnitRouteService freightUnitRouteService;

    @GetMapping
    public Route getByFreightUnitId(@RequestParam final String freightUnitId) {
        return freightUnitRouteService.getByTrackedProcessId(freightUnitId);
    }
}
