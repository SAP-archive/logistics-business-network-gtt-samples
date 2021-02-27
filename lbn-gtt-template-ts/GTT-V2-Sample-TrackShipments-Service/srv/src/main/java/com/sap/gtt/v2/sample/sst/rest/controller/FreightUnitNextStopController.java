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
 * {@link FreightUnitNextStopController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.FreightUnit} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + FreightUnitNextStopController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class FreightUnitNextStopController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/freightUnits/nextStop";

    @Autowired
    private NextStopService nextStopService;

    @GetMapping
    public NextStop getByFreightUnitId(@RequestParam final String freightUnitId) {
        return nextStopService.getByTrackedProcessId(freightUnitId).orElse(null);
    }
}
