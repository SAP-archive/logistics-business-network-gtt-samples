package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.TimeZoneList;
import com.sap.gtt.v2.sample.sst.rest.service.TimeZoneService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link TimeZoneController} is a controller which handles API requests.
 *
 * @author Min Li
 */
@RequestMapping(REST_ROOT_URL + TimeZoneController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class TimeZoneController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/timezones";

    @Autowired
    private TimeZoneService timeZoneService;

    @GetMapping()
    public TimeZoneList getAllTimeZones() {
        return timeZoneService.getAllTimeZones();
    }
}