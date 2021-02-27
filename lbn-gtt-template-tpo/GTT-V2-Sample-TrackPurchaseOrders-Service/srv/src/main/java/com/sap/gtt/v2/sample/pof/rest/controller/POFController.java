package com.sap.gtt.v2.sample.pof.rest.controller;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.domain.Route;
import com.sap.gtt.v2.sample.pof.odata.model.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem.FulfillmentStatus;
import com.sap.gtt.v2.sample.pof.rest.domain.timeline.TimelineEvent;
import com.sap.gtt.v2.sample.pof.rest.service.FulfillmentStatusService;
import com.sap.gtt.v2.sample.pof.rest.service.documentFlow.DocumentFlowService;
import com.sap.gtt.v2.sample.pof.rest.service.forward.ForwardService;
import com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow.FulfillmentProcessFlowService;
import com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline.TimeTrackingService;
import com.sap.gtt.v2.sample.pof.service.MapService;
import com.sap.gtt.v2.sample.pof.service.POFService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.json.JSONException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping(POFController.ROOT_URL)
public class POFController {
    public static final String ROOT_URL = "/sap/logistics/gtt/sample/pof/rest/v1";

    @Value("${HERE_MAP_KEY}")
    private String hereMapKey;

    private static final Logger logger = LoggerFactory.getLogger(POFController.class);

    @Autowired
    private POFService pofService;

    @Autowired
    private DocumentFlowService documentFlowService;

    @Autowired
    private FulfillmentProcessFlowService fulfillmentProcessFlowService;

    @Autowired
    private TimeTrackingService timeTrackingService;

    @Autowired
    private FulfillmentStatusService fulfillmentStatusService;

    @Autowired
    private ForwardService forwardService;

    @Autowired
    private MapService mapService;


    @PostMapping(value = "/forward")
    @ResponseStatus(HttpStatus.OK)
    public void forward(@RequestBody String body) {
        logger.info("Entering ForwardService with payload: {}", body);
        forwardService.execute(body);
    }

    @GetMapping(value = "/hello")
    public String hello() {
        return "hello";
    }

    @GetMapping(value = "/documentFlow")
    public ResponseEntity<DocumentFlow> getDocumentFlow(@RequestParam UUID purchaseOrderId) {
        return ResponseEntity.ok(documentFlowService.generateDocumentFlow(purchaseOrderId));
    }

    @GetMapping(value = "/documentItemFlow")
    public ResponseEntity<DocumentFlow> getDocumentItemFlow(@RequestParam UUID purchaseOrderId, @RequestParam UUID purchaseOrderItemId) {
        return ResponseEntity.ok(documentFlowService.generateDocumentItemFlow(purchaseOrderId, purchaseOrderItemId));
    }

    @GetMapping(value = "/fulfillmentProcessFlow")
    public ResponseEntity<FulfillmentProcessFlow> getFulfillmentProcessFlow(@RequestParam UUID purchaseOrderItemId) {
        return ResponseEntity.ok().body(fulfillmentProcessFlowService.generateFulfillmentProcessFlow(purchaseOrderItemId));
    }

    @GetMapping(value = "/fulfillmentStatus")
    public ResponseEntity<List<FulfillmentStatus>> getFulfillmentStatus(@RequestParam UUID inboundDeliveryItemId) {
        return ResponseEntity.ok(fulfillmentStatusService.getFulfillmentStatus(inboundDeliveryItemId));
    }

    @GetMapping(value = "/uiAnnotation", produces = MediaType.APPLICATION_XML_VALUE)
    public String getUiAnnotation() {
        return pofService.getUiAnnotation();
    }

    @GetMapping(path = "/i18n/{properties}", produces = MediaType.TEXT_PLAIN_VALUE + ";charset=UTF-8")
    public String getI18n(@PathVariable("properties") String properties) {
        return pofService.getI18n(properties);
    }

    @GetMapping(value = "/timelineEvent")
    public ResponseEntity<List<TimelineEvent>> getTimeLineEvent(@RequestParam final String deliveryItemId) {
        return ResponseEntity.ok().body(timeTrackingService.getByDeliveryItemId(deliveryItemId));
    }

    @GetMapping(value = "/hereMapKey", produces = "application/json")
    public String getHereMapKey() throws JSONException {
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("key", hereMapKey);
        return jsonObject.toString();
    }

    @GetMapping(value = "/routes")
    public ResponseEntity<List<Route>> getRoutes(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(mapService.getRoutes(deliveryItemId.toString()));
    }

    @GetMapping(value = "/carrierRefDocuments")
    public ResponseEntity<List<CarrierRefDocumentForDeliveryItem>> getCarrierRefDocuments(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(pofService.getCarrierRefDocuments(deliveryItemId));
    }
}
