package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import com.sap.gtt.v2.sample.sst.rest.service.FreightUnitEventsByStatusService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link FreightUnitEventsByStatusController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.FreightUnit} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + FreightUnitEventsByStatusController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class FreightUnitEventsByStatusController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/freightUnits/eventsByStatus";

    @Autowired
    private FreightUnitEventsByStatusService freightUnitEventsByStatusService;

    @GetMapping
    public List<EventsByStatus> getByFreightUnitId(@RequestParam final String freightUnitId) {
        return freightUnitEventsByStatusService.getByTrackedProcessId(freightUnitId);
    }
}
