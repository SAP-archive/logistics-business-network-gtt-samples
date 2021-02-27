package com.sap.gtt.v2.sample.sst.rest.controller;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.REST_ROOT_URL;

import com.sap.gtt.v2.sample.sst.common.model.CodeListValue;
import com.sap.gtt.v2.sample.sst.common.service.EventService;
import java.util.List;
import java.util.Locale;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * {@link EventController} is a controller which handles API requests.
 *
 * @author Aliaksandr Miron
 */
@RequestMapping(REST_ROOT_URL + EventController.CONTROLLER_ROOT_ENDPOINT)
@RestController
public class EventController {

    public static final String CONTROLLER_ROOT_ENDPOINT = "/events";

    @Autowired
    private EventService eventService;

    @GetMapping("/codeLists/{codeListName}")
    public List<CodeListValue> getCodeList(@PathVariable final String codeListName) {
        final Locale locale = LocaleContextHolder.getLocale();
        return eventService.getCodeList(codeListName, locale);
    }

    @PostMapping("/{eventType}")
    public void create(@RequestBody final String eventJson, @PathVariable final String eventType) {
        eventService.create(eventJson, eventType);
    }
}
