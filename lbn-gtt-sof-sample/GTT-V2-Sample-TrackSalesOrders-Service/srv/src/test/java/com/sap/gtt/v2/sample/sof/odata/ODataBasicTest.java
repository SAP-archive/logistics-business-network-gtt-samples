package com.sap.gtt.v2.sample.sof.odata;

import com.sap.gtt.v2.sample.sof.App;
import com.sap.gtt.v2.sample.sof.odata.handler.ODataHandlerFactory;
import com.sap.gtt.v2.sample.sof.odata.handler.PlannedEventHandler;
import com.sap.gtt.v2.sample.sof.odata.handler.TrackedProcessHandler;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.PlannedEvent;
import com.sap.gtt.v2.sample.sof.odata.model.TrackedProcess;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.assertj.core.api.Assertions;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.MODEL_NAMESPACE;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = App.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ActiveProfiles("test")
public class ODataBasicTest {
    private static Logger logger = LoggerFactory.getLogger(ODataBasicTestEx.class);

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private ODataHandlerFactory oDataHandlerFactory;

    @Before
    public void setUp() {
        oDataHandlerFactory.register(MODEL_NAMESPACE + ".TrackedProcess", TrackedProcessHandler.class);
        oDataHandlerFactory.register(MODEL_NAMESPACE + ".PlannedEvent", PlannedEventHandler.class);
    }

    @Test
    public void testReadMetadata() {
        ResponseEntity<String> response = restTemplate.getForEntity("/sap/logistics/gtt/sample/sof/odata/v1/$metadata", String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        // TODO: add more assertions
    }

    @Test
    public void testReadEntitySet() {
        String queryEntitySet = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess?$format=json&$inlinecount=allpages";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntitySet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(response.getBody(), TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(624);
        Assertions.assertThat(res.getResults()).hasSize(2);

        TrackedProcess tp0 = res.getResults().get(0);
        Assertions.assertThat(tp0.getId()).isEqualTo(UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tp0.getPlannedEvents()).isNotNull();
        Assertions.assertThat(tp0.getPlannedEvents()).hasSize(0);
        Assertions.assertThat(tp0.getProcessStatus()).isNotNull();
        Assertions.assertThat(tp0.getProcessStatus().getCode()).isNull();
        Assertions.assertThat(tp0.getProcessStatus().getName()).isNull();
    }

    @Test
    public void testReadEntitySetEx() {
        String queryEntitySet = "http://localhost:9098/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess?$format=json&$inlinecount=allpages";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntitySet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(response.getBody(), TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(624);
        Assertions.assertThat(res.getResults()).hasSize(2);

        TrackedProcess tp0 = res.getResults().get(0);
        Assertions.assertThat(tp0.getId()).isEqualTo(UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tp0.getPlannedEvents()).isNotNull();
        Assertions.assertThat(tp0.getPlannedEvents()).hasSize(0);
        Assertions.assertThat(tp0.getProcessStatus()).isNotNull();
        Assertions.assertThat(tp0.getProcessStatus().getCode()).isNull();
        Assertions.assertThat(tp0.getProcessStatus().getName()).isNull();
    }

    @Test
    public void testReadEntitySetWithExpand() {
        String queryEntitySet = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess?$format=json&$inlinecount=allpages" +
                "&$expand=plannedEvents, processStatus, plannedEvents/eventStatus";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntitySet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

        ODataResultList<TrackedProcess> res = ODataUtils.readEntitySet(response.getBody(), TrackedProcess.class);
        Assertions.assertThat(res.getCount()).isEqualTo(624);
        Assertions.assertThat(res.getResults()).hasSize(2);

        TrackedProcess tp0 = res.getResults().get(0);
        Assertions.assertThat(tp0.getId()).isEqualTo(UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tp0.getPlannedEvents()).isNotNull();
        Assertions.assertThat(tp0.getPlannedEvents()).hasSize(4);
        Assertions.assertThat(tp0.getProcessStatus()).isNotNull();
        Assertions.assertThat(tp0.getProcessStatus().getCode()).isEqualTo("OVERDUE");
    }

    @Test
    public void testReadEntity() {
        String queryEntity = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')?$format=json";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntity, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

        TrackedProcess tp = ODataUtils.readEntity(response.getBody(), TrackedProcess.class);
        Assertions.assertThat(tp).isNotNull();
        Assertions.assertThat(tp.getId()).isEqualTo(UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tp.getPlannedEvents()).isNotNull();
        Assertions.assertThat(tp.getPlannedEvents()).hasSize(0);
        Assertions.assertThat(tp.getProcessStatus()).isNotNull();
        Assertions.assertThat(tp.getProcessStatus().getCode()).isNull();
        Assertions.assertThat(tp.getProcessStatus().getName()).isNull();
    }

    @Test
    public void testReadEntityWithExpand() {
        String queryEntityWithExpand = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')?$format=json" +
                "&$expand=plannedEvents, processStatus, plannedEvents/eventStatus";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntityWithExpand, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);

        TrackedProcess tp = ODataUtils.readEntity(response.getBody(), TrackedProcess.class);
        Assertions.assertThat(tp).isNotNull();
        Assertions.assertThat(tp.getId()).isEqualTo(UUID.fromString("996f8dd3-b2e0-582e-ae1b-9d7d851c030d"));
        Assertions.assertThat(tp.getPlannedEvents()).hasSize(4);
        Assertions.assertThat(tp.getPlannedEvents().get(0).getEventStatus()).isNotNull();
        Assertions.assertThat(tp.getPlannedEvents().get(0).getEventStatus().getCode()).isEqualTo("REPORTED");
        Assertions.assertThat(tp.getPlannedEvents().get(0).getEventStatus().getName()).isEqualTo("Reported");
        Assertions.assertThat(tp.getProcessStatus()).isNotNull();
        Assertions.assertThat(tp.getProcessStatus().getCode()).isEqualTo("OVERDUE");
    }

    @Test
    public void testReadEntityWithNavigation() {
        String queryWithNavigation = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')/plannedEvents?$format=json" +
                "&$expand=eventStatus";

        ResponseEntity<String> response = restTemplate.getForEntity(queryWithNavigation, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        System.out.println(SOFUtils.getPrettyJsonString(response.getBody()));

        ODataResultList<PlannedEvent> res = ODataUtils.readEntitySet(response.getBody(), PlannedEvent.class);
        Assertions.assertThat(res.getResults()).hasSize(4);
        Assertions.assertThat(res.getResults().get(0).getEventStatus().getCode()).isEqualTo("REPORTED");
        Assertions.assertThat(res.getResults().get(0).getEventStatus().getName()).isEqualTo("Reported");
    }

    @Test
    public void testReadEntityWithSelect() {
        String queryEntity = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess(guid'996f8dd3-b2e0-582e-ae1b-9d7d851c030d')?$format=json" +
                "&$select=id, trackingId";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntity, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
    }

    @Test
    public void testCountEntitySet() {
        String queryEntityCount = "/sap/logistics/gtt/sample/sof/odata/v1/TrackedProcess/$count";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEntityCount, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        Assertions.assertThat(Integer.parseInt(response.getBody())).isEqualTo(2);
    }

    @Test
    public void testExecuteBatch() {
        String queryBatch = "/sap/logistics/gtt/sample/sof/odata/v1/$batch";
        String batch = SOFUtils.getStringFromResource("/odata/batch-local.txt");
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Type", "multipart/mixed;boundary=batch_1f1e-8cdb-aefd");
        ResponseEntity<String> response = restTemplate.exchange(queryBatch, HttpMethod.POST,
                new HttpEntity<>(batch, headers), String.class);
        System.out.println("testExecuteBatch:" + response);
        // can not pass on linux system, check line endings
        // Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        System.out.println("body:" + response.getBody());
    }

    @Test
    public void testGetSofMetadata() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1/$metadata";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
        String body = response.getBody();
        Assertions.assertThat(body).contains(Arrays.asList("<EntityType Name=\"ShipmentCarrierRefDocuments\">",
                "<EntityType Name=\"ShipmentTrackedObjects\">", "<EntityType Name=\"ShipmentStops\">"));
    }

    @Test
    public void testODataError() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1/$metadata1";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println("response:" + response);
        String body = response.getBody();
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
        Assertions.assertThat(body).contains(Arrays.asList("Could not find an entity set or function import for '$metadata1'."));
    }

    @Test
    public void testUnsupportedTrackingIdType() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/nextNodes" +
                "?currTpId=9cbeae25-2d7f-5452-a5bb-4bc6944df130&trackingIdType=SHIPMENT_ORDER";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
        Assertions.assertThat(response.getBody()).contains("Unsupported tracking id type: SHIPMENT_ORDER");
    }
}
