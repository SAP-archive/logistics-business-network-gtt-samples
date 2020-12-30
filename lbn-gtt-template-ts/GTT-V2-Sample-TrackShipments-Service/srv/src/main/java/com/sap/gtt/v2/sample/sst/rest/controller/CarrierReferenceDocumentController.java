package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import com.sap.gtt.v2.sample.sst.rest.service.CarrierReferenceDocumentService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link CarrierReferenceDocumentController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + CarrierReferenceDocumentController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class CarrierReferenceDocumentController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/carrierRefDocuments";

    @Autowired
    private CarrierReferenceDocumentService carrierReferenceDocumentService;

    @GetMapping
    public List<CarrierReferenceDocumentDto> getByShipmentId(@RequestParam final String shipmentId) {
        return carrierReferenceDocumentService.getByShipmentId(shipmentId);
    }
}
