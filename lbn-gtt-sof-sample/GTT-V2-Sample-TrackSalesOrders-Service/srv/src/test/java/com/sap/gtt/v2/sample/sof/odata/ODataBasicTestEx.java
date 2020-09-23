package com.sap.gtt.v2.sample.sof.odata;

import com.sap.gtt.v2.sample.sof.App;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.*;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.assertj.core.api.Assertions;
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
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Arrays;
import java.util.Locale;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = App.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ActiveProfiles("test")
public class ODataBasicTestEx {

    private static Logger logger = LoggerFactory.getLogger(ODataBasicTestEx.class);

    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    public void testReadSalesOrderSet() {
        String querySalesOrderSet = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder?$format=json&$inlinecount=allpages&$top=2" +
                "&$expand=salesOrderItemTPs, salesOrderItemTPs/salesOrderItem, incoterms";
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderSet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        ODataResultList<SalesOrder> res = ODataUtils.readEntitySet(response.getBody(), SalesOrder.class);
        if (!res.getResults().isEmpty()) {
            SalesOrder salesOrder = res.getResults().get(0);
            testReadSalesOrderEntity(salesOrder);
        }
    }

    private void testReadSalesOrderEntity(SalesOrder salesOrder) {
        String querySalesOrderEntity = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder(guid'to-be-replaced')?$format=json" +
                "&$expand=salesOrderItemTPs, salesOrderItemTPs/salesOrderItem, incoterms";
        querySalesOrderEntity = querySalesOrderEntity.replace("to-be-replaced", salesOrder.getId().toString());
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderEntity, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();

        SalesOrder salesOrder1 = ODataUtils.readEntity(response.getBody(), SalesOrder.class);
        Assertions.assertThat(salesOrder1.getId()).isEqualTo(salesOrder.getId());
        Assertions.assertThat(salesOrder1.getCurrency()).isEqualTo(salesOrder.getCurrency());

    }

    @Test
    public void testCountSalesOrder() {
        String querySalesOrderCount = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder/$count";
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderCount, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        if (response.getStatusCode().is2xxSuccessful()) {
            Integer count = Integer.parseInt(response.getBody());
            Assertions.assertThat(count).isNotNull();
        }
    }

    @Test
    public void testReadSalesOrderItemSet() {
        String querySalesOrderItemSet = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem?$format=json&$top=2";
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderItemSet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        ODataResultList<SalesOrderItem> res = ODataUtils.readEntitySet(response.getBody(), SalesOrderItem.class);
        if (!res.getResults().isEmpty()) {
            SalesOrderItem salesOrderItem = res.getResults().get(0);
            testReadSalesOrderItemEntity(salesOrderItem);
        }
    }

    private void testReadSalesOrderItemEntity(SalesOrderItem salesOrderItem) {
        String querySalesOrderItem = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem(guid'to-be-replaced')?$format=json" +
                "&expand=processStatus";
        querySalesOrderItem = querySalesOrderItem.replace("to-be-replaced", salesOrderItem.getId().toString());
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderItem, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();

        SalesOrderItem salesOrderItem1 = ODataUtils.readEntity(response.getBody(), SalesOrderItem.class);
        Assertions.assertThat(salesOrderItem1.getId()).isEqualTo(salesOrderItem.getId());
        Assertions.assertThat(salesOrderItem1.getCurrency()).isEqualTo(salesOrderItem.getCurrency());

    }

    @Test
    public void testCountSalesOrderItem() {
        String querySalesOrderItemCount = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem/$count";
        ResponseEntity<String> response = restTemplate.getForEntity(querySalesOrderItemCount, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        if (response.getStatusCode().is2xxSuccessful()) {
            Integer count = Integer.parseInt(response.getBody());
            Assertions.assertThat(count).isNotNull();
        }
    }

    @Test
    public void testDefaultHandler() {
        String queryEventCount = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/Event/$count";
        ResponseEntity<String> response = restTemplate.getForEntity(queryEventCount, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isNotNull();
        if (response.getStatusCode().is2xxSuccessful()) {
            Integer count = Integer.parseInt(response.getBody());
            Assertions.assertThat(count).isNotNull();
        }

        String queryEventSet = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/Event?$inlinecount=allpages&$top=2";
        response = restTemplate.getForEntity(queryEventSet, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        if (response.getStatusCode().is2xxSuccessful()) {
            ODataResultList<Event> res = ODataUtils.readEntitySet(response.getBody(), Event.class);
            if (res.getResults().size() > 0) {
                String queryEvent = "/sap/logistics/gtt/sample/sof/odata/v1" +
                        "/Event(guid'to-be-replaced')?$format=json&";
                Event event = res.getResults().get(0);
                queryEvent = queryEvent.replace("to-be-replaced", event.getId().toString());
                response = restTemplate.getForEntity(queryEvent, String.class);
                System.out.println(response);
                Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
            }
        }
    }

    @Test
    public void testOrderBy() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder?$skip=0&$top=20&$orderby=lastChangeDateTime desc&$inlinecount=allpages";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Test
    public void testBatch() {

        String batch = SOFUtils.getStringFromResource("/odata/batch-read-service.txt");

        String query = "/sap/logistics/gtt/sample/sof/odata/v1/$batch";
        HttpHeaders headers = new HttpHeaders();
        headers.set("Content-Type", "multipart/mixed;boundary=batch_7c1a-2423-f901");
        ResponseEntity<String> response = restTemplate.exchange(query, HttpMethod.POST,
                new HttpEntity<>(batch, headers), String.class);
        System.out.println("testExecuteBatch:" + response);
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Autowired
    SOFService sofService;

    @Test
    public void testSendToWriteService() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem?$skip=0&$top=20&$inlinecount=allpages&$expand=salesOrder";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        ODataResultList<SalesOrderItem> res = ODataUtils.readEntitySet(response.getBody(), SalesOrderItem.class);
        if (!res.getResults().isEmpty()) {
            SalesOrderItem salesOrderItem = res.getResults().get(0);
            if (salesOrderItem.getId() != null && salesOrderItem.getSalesOrder() != null) {
                sofService.updateCompletionAndDelayedQuantities(salesOrderItem.getId().toString());
            }
        }
    }

    @Test
    public void testGetUiAnnotation() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/uiAnnotation";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Test
    public void testGetI18n() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/i18n/i18n-dummy.properties";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();

        query = "/sap/logistics/gtt/sample/sof/rest/v1/i18n/i18n_en.properties";
        response = restTemplate.getForEntity(query, String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    @Test
    public void testGetLocation() {
        //locationService = "https://lbn-gtt-samples.gtt-flp-lbnplatform-int.cfapps.sap.hana.ondemand.com/api/location/v1/Location?$format=json&$inlinecount=allpages&$filter=locationAltKey%20eq%20'xri://sap.com/id:LBN%2310010001006:INT_TEST_CORE_ENGINE:Location:Customer:%7B%7BEXTERNALID_COREENGINE%7D%7D'";
        String locationAltKey = "xri://sap.com/id:LBN#10010001006:INT_TEST_CORE_ENGINE:Location:Customer:{{EXTERNALID_COREENGINE}}";

        Location location = gttCoreServiceClient.getLocation(locationAltKey);
        System.out.println(SOFUtils.getGson().toJson(location));

        locationAltKey = "no-such-key";
        location = gttCoreServiceClient.getLocation(locationAltKey);
        System.out.println(SOFUtils.getGson().toJson(location));
        Assertions.assertThat(location).isNull();
    }

    @Test
    public void testGetCodeListWithLang() {
        String query = "/ExecutionStatus?$expand=localized";
        HttpHeaders headers = new HttpHeaders();
        headers.setAcceptLanguageAsLocales(Arrays.asList(Locale.SIMPLIFIED_CHINESE));

        ODataResultList<ExecutionStatus> res = gttCoreServiceClient.readEntitySet(query, ExecutionStatus.class, headers);
        System.out.println(res);

    }

    @Test
    public void testGetHereMapKey() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/hereMapKey";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
        Assertions.assertThat(response.getBody()).isEqualTo("{\"key\":\"6OYdhmagtGi6hns0-O56oK6gy1QxujVp1peN0XeN4Sc\"}");
    }

    @Test
    public void testForwardReceived() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/forward";
        String payload = SOFUtils.getStringFromResource("/odata/payload-received-shipment-pod.json");

        ResponseEntity<String> response = restTemplate.exchange(query, HttpMethod.POST, new HttpEntity<>(payload, null), String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isFalse();
    }

    @Test
    public void testIncoterms() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/Incoterms('CRF%2F')"; // invalid key value

        query = UriComponentsBuilder.fromPath(query)
                .encode().toUriString();
        System.out.println(query);

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode().is5xxServerError()).isTrue();
    }

    @Test
    public void testGetCarrierRefDocuments() {
        // has carrierRefDocuments
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/carrierRefDocuments?deliveryItemId=b8b7c6c9-9a3f-5299-9a73-13f21f071d4f";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);

        // doesn't have
        query = "/sap/logistics/gtt/sample/sof/rest/v1/carrierRefDocuments?deliveryItemId=ac3ae0c3-08de-57c1-9c5a-c6fcfcd1c871";
        response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testGetFulfillmentStatus() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/fulfillmentStatus?deliveryItemId=f970c946-0089-5488-9b13-6077a1d32b0a";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testUpdateLastActivityOfDeliveryItem() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/forward";
        String payload = SOFUtils.getStringFromResource("/odata/delivery-item-received.json");

        ResponseEntity<String> response = restTemplate.exchange(query, HttpMethod.POST, new HttpEntity<>(payload, null), String.class);
        System.out.println(response.getBody());
        Assertions.assertThat(response.getStatusCode().is2xxSuccessful()).isTrue();
    }

    @Test
    public void testGetInitialNodesStartsFromShipment() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/initialNodes" +
                "?deliveryItemId=538c94f2-36a6-5d8c-9092-e63a270566c1&plannedEventId=4e2072ef-e5b2-11ea-8c4e-2d7416acb414";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testGetInitialNodesStartsFromDelivery() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/initialNodes" +
                "?deliveryItemId=01ad6cfd-1cf4-559b-80d1-5a3910bcfb74&plannedEventId=b874a90a-f3fd-11ea-bbeb-9fbe6ce1a21d";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testGetInitialNodesStartsFromDeliveryItem() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/initialNodes" +
                "?deliveryItemId=d10d4e84-d7f1-55bf-a449-1cff32adaefd&plannedEventId=092ddc3c-f3d7-11ea-bbeb-09888ad1bb65";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testGetNextNodes() {
        String query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/nextNodes" +
                "?currTpId=9cbeae25-2d7f-5452-a5bb-4bc6944df130&trackingIdType=OUTBOUND_DELIVERY";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);

        query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/nextNodes" +
                "?currTpId=5928e41b-1096-5805-b074-b1914bb0843b&trackingIdType=OUTBOUND_DELIVERY_IT";

        response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);

        // test with another sales order item
        query = "/sap/logistics/gtt/sample/sof/rest/v1/impactAnalysis/nextNodes" +
                "?currTpId=90d286ee-31f9-547e-a4c4-056efc2137a0&trackingIdType=SALES_ORDER_ITEM";

        response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testCallReadServiceFailed() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder(guid'94a2f0ba-f014-11ea-bbdd-ed4e884ce938')";
        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
        Assertions.assertThat(response.getStatusCode()).isEqualTo(HttpStatus.INTERNAL_SERVER_ERROR);
        Assertions.assertThat(response.getBody()).contains("Call read service failed. Contact your System Administrator.");
    }

    @Test
    public void testLastActivityFieldsOfDeliveryItem() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/DeliveryItem(guid'cc4ee046-ccf9-5900-98e6-706d616e457b')?$expand=lastVPLocationType";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);

        query = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/DeliveryItem?$expand=lastVPLocationType&$filter=id eq guid'cc4ee046-ccf9-5900-98e6-706d616e457b'";
        response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testDeliveryItemArrivalTimesAcc() {
        String query = "/sap/logistics/gtt/sample/sof/odata/v1/DeliveryItem?$skip=0&$top=100&$orderby=deliveryNo asc,itemNo asc,itemNo asc" +
                "&$filter=salesOrderItem_id eq guid'3364c717-d53c-502c-abf3-3a6a60df8295'" +
                "&$select=deliveryNo, itemNo,initialPlannedDate,revisedPlannedDate,orderQuantity,quantityUoM,processStatus_code, processStatus/name, processStatus/localized/name,executionStatus_code, executionStatus/name, executionStatus/localized/name, lastEventName, lastLocationDescription, lastVPLocationType_code, lastVPLocationType/name, lastVPLocationType/localized/name,arrivalTimes,id" +
                "&$expand=processStatus/localized, lastVPLocationType/localized,executionStatus/localized,arrivalTimes&$inlinecount=allpages";

        ResponseEntity<String> response = restTemplate.getForEntity(query, String.class);
        System.out.println(response);
    }

    @Test
    public void testReadEntitySetAll() {
        String query = "/TrackedProcess?$filter=trackedProcessType eq 'com.lbngttsamples.gtt.app.sof.SalesOrder.SalesOrder'";
        HttpHeaders headers = new HttpHeaders();
        headers.setAcceptLanguageAsLocales(Arrays.asList(Locale.SIMPLIFIED_CHINESE));

        ODataResultList<TrackedProcess> res = gttCoreServiceClient.readEntitySetAll(query, TrackedProcess.class, headers);
        Assertions.assertThat(res.getCount()).isEqualTo(res.getResults().size());
        System.out.println(res.getCount());
    }
}
