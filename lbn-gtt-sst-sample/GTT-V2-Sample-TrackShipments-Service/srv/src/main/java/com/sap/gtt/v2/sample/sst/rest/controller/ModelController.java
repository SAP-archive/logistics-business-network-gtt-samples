package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.rest.model.AdmissibleUnplannedEvent;
import com.sap.gtt.v2.sample.sst.rest.service.ModelService;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link ModelController} is a controller which handles API requests.
 *
 * @author Min Li
 */
@RequestMapping(REST_ROOT_URL + ModelController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class ModelController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/model";

    @Autowired
    private ModelService modelService;

    @GetMapping("{trackedProcess}/unplannedEvents")
    public List<AdmissibleUnplannedEvent> getUnplannedEventTypesOfTp(@PathVariable final String trackedProcess) {
        return modelService.getUnplannedEventTypesOfTp(trackedProcess);
    }
}