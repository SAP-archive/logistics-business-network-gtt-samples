package com.sap.gtt.v2.sample.sof.rest.controller.service;


import com.fasterxml.jackson.annotation.ObjectIdGenerators;

import com.google.gson.Gson;

import com.sap.gtt.v2.sample.sof.domain.EventEx;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.odata.model.Delivery;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.odata.model.Shipment;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.Route;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.map.SideContent;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.platform.commons.util.CollectionUtils;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.util.*;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.powermock.api.mockito.PowerMockito.mockStatic;


@RunWith(PowerMockRunner.class)
@PrepareForTest(SOFUtils.class)
public class MapServiceTest {
    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private MapService mapService;
    @Mock
    private SOFService sofService;

    @Test
    public void getRoutesTest() throws IOException {

        String deliveryItemId = "2ae61e82-0af3-518f-b20d-fd2ca06b5cff";
        String pedUrl = "/ProcessEventDirectory?&$filter= (process_id eq guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff') and ( correlationType_code eq 'EARLY_REPORTED' or correlationType_code eq 'REPORTED' or correlationType_code eq 'LATE_REPORTED' or correlationType_code eq 'UNPLANNED' or correlationType_code eq 'UNPLANNED_DELAYED' or correlationType_code eq 'UNPLANNED_ONTIME' ) &$expand=event , plannedEvent &$orderby=event/actualBusinessTimestamp ";

        String pedJson = IOUtils.toString(new ClassPathResource("/odata/ped-route.json").getInputStream());
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(pedJson, ProcessEventDirectory.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(pedUrl, ProcessEventDirectory.class)).thenReturn(
                processEventDirectoryODataResultList);

        String goodsIssuedJsonUrl = "/GoodsIssued?&$filter= (id eq guid'cf32a237-e760-11ea-b9d1-df5840db32fa') ";
        String goodsIssuedJson = IOUtils.toString(new ClassPathResource("/odata/goodsIssued-route.json").getInputStream());
        ODataResultList<EventEx> goodsIssued = ODataUtils.readEntitySet(goodsIssuedJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(goodsIssuedJsonUrl, EventEx.class)).thenReturn(
                goodsIssued);


        String pickingUrl = "/Picking?&$filter= (id eq guid'cb2186b0-e760-11ea-b9d1-493ddc8ffb9a') ";
        String pickingJson = IOUtils.toString(new ClassPathResource("/odata/picking-route.json").getInputStream());
        ODataResultList<EventEx> picking = ODataUtils.readEntitySet(pickingJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(pickingUrl, EventEx.class)).thenReturn(
                picking);

        String delayUrl = "/Delay?&$filter= (id eq guid'913570d5-ea85-11ea-b9d1-57ee9073f52e') &$expand=estimatedArrival ";
        String delayJson = IOUtils.toString(new ClassPathResource("/odata/delay-route.json").getInputStream());
        ODataResultList<EventEx> delay = ODataUtils.readEntitySet(delayJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(delayUrl, EventEx.class)).thenReturn(
                delay);

        String plannedEventUrl = "/PlannedEvent?&$filter= (((eventMatchKey eq '00000020900003') and (eventType eq 'com.lbngttsamples.gtt.app.sof.Shipment.Arrival')) and (process_id eq guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff')) ";
        String plannedEventJson = IOUtils.toString(new ClassPathResource("/odata/planned-event-route.json").getInputStream());
        ODataResultList<PlannedEvent> plannedEvent = ODataUtils.readEntitySet(plannedEventJson, PlannedEvent.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(plannedEventUrl, PlannedEvent.class)).thenReturn(
                plannedEvent);

        String locationAltKey = "xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:LogisticLocation:NEWYORK";
        String locationJson = IOUtils.toString(new ClassPathResource("/odata/location-route.json").getInputStream());
        ODataResultList<Location> locationODataResultList = ODataUtils.readEntitySet(locationJson, Location.class);
        Mockito.when(gttCoreServiceClient.getLocation(locationAltKey)).thenReturn(locationODataResultList.getResults().get(0));


        String plannedEventJson1 = IOUtils.toString(new ClassPathResource("/odata/plannedEvent-route.json").getInputStream());
        ODataResultList<PlannedEvent> plannedEvent1 = ODataUtils.readEntitySet(plannedEventJson1, PlannedEvent.class);
        Mockito.when(sofService.getPlannedEvents4TP(UUID.fromString(deliveryItemId))).thenReturn(plannedEvent1.getResults());


        String locationsJson = IOUtils.toString(new ClassPathResource("/odata/locations-route.json").getInputStream());
        ODataResultList<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class);
        Mockito.when(gttCoreServiceClient.getLocations(anySet())).thenReturn(locations.getResults());

        String shipmentUrl = "/Shipment?&$filter= (altKey eq 'xri://sap.com/id:LBN#10010001016:QM7CLNT910:SHIPMENT_ORDER:0000002090') &$expand=stopsForVP ";
        String shipmentJson = IOUtils.toString(new ClassPathResource("/odata/shipment-route.json").getInputStream());
        ODataResultList<Shipment> shipment = ODataUtils.readEntitySet(shipmentJson, Shipment.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(shipmentUrl, Shipment.class)).thenReturn(
                shipment);

        String deliveryItemUrl = "/DeliveryItem(guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff')";
        String deliveryItemJson = IOUtils.toString(new ClassPathResource("/odata/deliveryItem-route.json").getInputStream());
        DeliveryItem deliveryItem = ODataUtils.readEntity(deliveryItemJson,DeliveryItem.class);
        Mockito.when(gttCoreServiceClient.readEntity(deliveryItemUrl, DeliveryItem.class)).thenReturn(
                deliveryItem);

        String locationAltKey1 = "xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:Customer:LBN_CUS_CL";
        String locationJson1 = IOUtils.toString(new ClassPathResource("/odata/location1-route.json").getInputStream());
        ODataResultList<Location> locationODataResultList1 = ODataUtils.readEntitySet(locationJson1, Location.class);
        Mockito.when(gttCoreServiceClient.getLocation(locationAltKey1)).thenReturn(locationODataResultList1.getResults().get(0));

        String getDepartureEventUrl = "/Departure?$filter=(id eq guid'fb122676-ea82-11ea-b9d1-bda152ca4d8c') or (id eq guid'71dd2c10-e763-11ea-b9d1-5ba2a71155f8')";
        String departureEventJson = IOUtils.toString(new ClassPathResource("/odata/departure-event-route.json").getInputStream());
        ODataResultList<EventEx> departureEvent = ODataUtils.readEntitySet(departureEventJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(getDepartureEventUrl, EventEx.class)).thenReturn(
                departureEvent);



        String getArrivalEventUrl = "/Arrival?$filter=id eq guid'b524a419-e762-11ea-b9d1-e5a04a2b62ad'";
        String arrivalEventJson = IOUtils.toString(new ClassPathResource("/odata/arrival-event-route.json").getInputStream());
        ODataResultList<EventEx> arrivalEvent = ODataUtils.readEntitySet(arrivalEventJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(getArrivalEventUrl, EventEx.class)).thenReturn(
                arrivalEvent);

        List<Route> routes = mapService.getRoutes(deliveryItemId);

        Assert.assertEquals(3,routes.size());
    }
    @Test
    public void testGenerateSideContentUrl() {
        String deliveryItemId = "****-*****";
        String altKey = "hello://test#altKey";
        String url = mapService.generateContentSideUrl(deliveryItemId,altKey);
        String expected="/ProcessEventDirectory?&$filter= ((process_id eq guid'****-*****') and (event/altKey eq 'hello://test#altKey')) and ( correlationType_code eq 'EARLY_REPORTED' or correlationType_code eq 'REPORTED' or correlationType_code eq 'LATE_REPORTED' or correlationType_code eq 'UNPLANNED' ) &$expand=event , plannedEvent &$orderby=event/actualBusinessTimestamp desc";
        Assert.assertEquals(expected,url);
        altKey = "hello://SHIPMENT_ORDER#altKey";
        url = mapService.generateContentSideUrl(deliveryItemId,altKey);
        expected = "/ProcessEventDirectory?&$filter= (process_id eq guid'****-*****') and ( correlationType_code eq 'EARLY_REPORTED' or correlationType_code eq 'REPORTED' or correlationType_code eq 'LATE_REPORTED' or correlationType_code eq 'UNPLANNED' ) and (substringof('hello://RESOURCE#altKey',event/altKey) or (event/altKey eq 'hello://SHIPMENT_ORDER#altKey')) &$expand=event , plannedEvent &$orderby=event/actualBusinessTimestamp desc";
        Assert.assertEquals(expected,url);
    }

    @Test
    public void testGetSideContents() throws IOException {
        String altKey = "xri://sap.com/id:LBN#10010001016:QM7CLNT910:SHIPMENT_ORDER:0000002090";
        String deliveryItemId = "2ae61e82-0af3-518f-b20d-fd2ca06b5cff";
        String eventMatchKey = "0000002090";
        String plannedEventId = "8eeb8e94-e760-11ea-b9d1-cf9f4484a832";
        String pedJson = IOUtils.toString(new ClassPathResource("/odata/ped-sideContent.json").getInputStream());
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(pedJson, ProcessEventDirectory.class);
        Mockito.when(gttCoreServiceClient.readEntitySet(anyString(), eq(ProcessEventDirectory.class))).thenReturn(
                processEventDirectoryODataResultList);
        String plannedEventJson = IOUtils.toString(new ClassPathResource("/odata/plannedEvent-sideContent.json").getInputStream());
        ODataResultList<PlannedEvent> plannedEventODataResultList = ODataUtils.readEntitySet(plannedEventJson, PlannedEvent.class);
        String generatePlannedEventUrl = "/PlannedEvent?&$filter= (process_id eq guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff') and (substringof('0000002090',eventMatchKey)) &$orderby=eventMatchKey desc,plannedBusinessTimestamp desc,payloadSequence desc";
        Mockito.when(gttCoreServiceClient.readEntitySet(generatePlannedEventUrl,PlannedEvent.class)).thenReturn(plannedEventODataResultList);

        List<SideContent> sideContents = mapService.getSideContents(deliveryItemId,altKey,eventMatchKey,plannedEventId);
        Assert.assertEquals(7,sideContents.size());
        Assert.assertEquals("POD",sideContents.get(0).getEventType());
    }
}
