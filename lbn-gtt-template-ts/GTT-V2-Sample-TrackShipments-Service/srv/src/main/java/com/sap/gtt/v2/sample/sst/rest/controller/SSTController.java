package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.http.MediaType.APPLICATION_XML_VALUE;
import static org.springframework.http.MediaType.TEXT_PLAIN_VALUE;

import com.sap.gtt.v2.sample.sst.rest.service.SSTService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link SSTController} is a controller which handles API requests.
 *
 * @author Min Li
 */
@RequestMapping(REST_ROOT_URL)
@RestController
public class SSTController {

    @Autowired
    private SSTService sstService;

    @GetMapping(value = "/uiAnnotation", produces = APPLICATION_XML_VALUE)
    public String getUiAnnotation() {
        return sstService.getUiAnnotation();
    }

    @GetMapping(path = "/i18n/{properties}", produces = TEXT_PLAIN_VALUE + ";charset=UTF-8")
    public String getI18n(@PathVariable("properties") String properties) {
        return sstService.getI18n(properties);
    }

    @GetMapping(value = "/hereMapKey", produces = APPLICATION_JSON_VALUE)
    public String getHereMapKey() {
        return sstService.getHereMapKey();
    }
}
