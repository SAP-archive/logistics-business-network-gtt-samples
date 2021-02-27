package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.EventsByStatus;
import com.sap.gtt.v2.sample.sst.rest.service.ShipmentEventsByStatusService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link ShipmentEventsByStatusController} is a controller which handles API requests
 * for {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + ShipmentEventsByStatusController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class ShipmentEventsByStatusController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/shipments/eventsByStatus";

    @Autowired
    private ShipmentEventsByStatusService shipmentEventsByStatusService;

    @GetMapping
    public List<EventsByStatus> getByShipmentId(@RequestParam final String shipmentId) {
        return shipmentEventsByStatusService.getByTrackedProcessId(shipmentId);
    }
}
