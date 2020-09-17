package com.sap.gtt.v2.sample.sof.rest.controller;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.sof.domain.EventEx;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.FulfillmentStatus;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Node;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow.ExecutionFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.Route;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.SideContent;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.service.DocumentFlowService;
import com.sap.gtt.v2.sample.sof.rest.controller.service.ExecutionFlowService;
import com.sap.gtt.v2.sample.sof.rest.controller.service.ImpactAnalysisService;
import com.sap.gtt.v2.sample.sof.rest.controller.service.MapService;
import com.sap.gtt.v2.sample.sof.rest.controller.service.FulfillmentProcessFlowService;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ExecutorService;

@RestController
@RequestMapping(SOFController.ROOT_URL)
public class SOFController {
    public static final String ROOT_URL = "/sap/logistics/gtt/sample/sof/rest/v1";

    private static final Logger logger = LoggerFactory.getLogger(SOFController.class);

    @Value("${HERE_MAP_KEY}")
    private String hereMapKey;

    @Autowired
    private DocumentFlowService documentFlowService;

    @Autowired
    private ExecutionFlowService executionFlowService;

    @Autowired
    private FulfillmentProcessFlowService fulfillmentProcessFlowService;

    @Autowired
    private SOFService sofService;

    @Autowired
    private MapService mapService;

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Autowired
    private ExecutorService executorService;

    @Autowired
    private ImpactAnalysisService impactAnalysisService;

    @GetMapping(value = "/hello")
    public String hello()  {
        return "hello";
    }

    @GetMapping(value = "/documentFlow")
    public ResponseEntity<DocumentFlow> getDocumentFlow(@RequestParam UUID salesOrderId) {
        return ResponseEntity.ok().body(documentFlowService.generateDocumentFlow(salesOrderId));
    }

    @PostMapping(value = "/forward")
    public ResponseEntity<String> forward(@RequestBody String requestBody)  {
        logger.info("forwarded tp received: {}", requestBody);

        sofService.executeTasks(requestBody);

        return ResponseEntity.ok("");
    }

    @GetMapping(value = "/uiAnnotation", produces = MediaType.APPLICATION_XML_VALUE)
    public String getUiAnnotation() {
        String uiAnnotation = gttCoreServiceClient.getUiAnnotation();
        return validateAndReturn(uiAnnotation);
    }

    private String validateAndReturn(String uiAnnotation) {
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("key", uiAnnotation);
        return jsonObject.get("key").getAsString();
    }

    @GetMapping(path = "/i18n/{properties}", produces = MediaType.TEXT_PLAIN_VALUE + ";charset=UTF-8")
    public String getI18n(@PathVariable("properties") String properties) {
        return sofService.getI18n(properties);
    }

    @GetMapping(value = "/executionFlow")
    public ResponseEntity<ExecutionFlow> getExecutionFlow(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(executionFlowService.generateExecutionFlow(deliveryItemId));
    }

    @GetMapping(value = "/eventReportHistory")
    public ResponseEntity<List<EventEx>> getEventReportHistory(@RequestParam UUID eventId) {
        return ResponseEntity.ok().body(executionFlowService.getEventReportHistory(eventId));
    }

    @GetMapping(value = "/hereMapKey")
    public String getHereMapKey() throws JSONException {
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("key", hereMapKey);
        return jsonObject.toString();
    }

    @GetMapping(value = "/routes")
    public ResponseEntity<List<Route>> getRoutes(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(mapService.getRoutes(deliveryItemId.toString()));
    }

    @GetMapping(value = "/sideContent")
    public ResponseEntity<List<SideContent>> getSideContent(@RequestParam UUID deliveryItemId,@RequestParam(required = false) String altKey,@RequestParam(required = false) UUID plannedEventId,@RequestParam(required = false) String eventMatchKey) throws UnsupportedEncodingException {
        if(StringUtils.isNotBlank(altKey)) {
            altKey = URLDecoder.decode(altKey, "utf-8");
        }
        return ResponseEntity.ok().body(mapService.getSideContents(deliveryItemId.toString(),altKey,eventMatchKey,plannedEventId==null?null:plannedEventId.toString()));
    }

    @GetMapping(value = "/carrierRefDocuments")
    public ResponseEntity<List<CarrierRefDocumentForDeliveryItem>> getCarrierRefDocuments(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(sofService.getCarrierRefDocuments(deliveryItemId));
    }

    @GetMapping(value = "/fulfillmentStatus")
    public ResponseEntity<List<FulfillmentStatus>> getFulfillmentStatus(@RequestParam UUID deliveryItemId) {
        return ResponseEntity.ok().body(sofService.getFulfillmentStatus(deliveryItemId));
    }

    @GetMapping(value = "/impactAnalysis/initialNodes")
    public ResponseEntity<DocumentFlow> getInitialNodes(@RequestParam UUID deliveryItemId, @RequestParam UUID plannedEventId) {
        return ResponseEntity.ok().body(impactAnalysisService.getInitialNodes(deliveryItemId, plannedEventId));
    }

    @GetMapping(value = "/impactAnalysis/nextNodes")
    public ResponseEntity<Map<String, List<Node>>> getNextNodes(@RequestParam UUID currTpId, @RequestParam String trackingIdType) {
        return ResponseEntity.ok().body(
                Collections.singletonMap("nodes", impactAnalysisService.getNextNodes(currTpId, trackingIdType)));
    }

    @GetMapping(value = "/fulfillmentProcessFlow")
    public ResponseEntity<FulfillmentProcessFlow> getFulfillmentProcessFlow(@RequestParam UUID salesOrderItemId) {
        return ResponseEntity.ok().body(fulfillmentProcessFlowService.generateFulfillmentProcessFlow(salesOrderItemId));
    }

}
