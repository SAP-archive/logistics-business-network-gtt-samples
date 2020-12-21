package com.sap.gtt.v2.sample.pof.rest.service;


import com.sap.gtt.v2.sample.pof.domain.*;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.Shipment;
import com.sap.gtt.v2.sample.pof.service.MapService;
import com.sap.gtt.v2.sample.pof.service.POFService;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.*;


@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class MapServiceTest {
    @Mock
    private GTTCoreServiceClient gttCoreServiceClient;
    @InjectMocks
    private MapService mapService;
    @Mock
    private POFService pofService;

    @Test
    public void getRoutesTest() throws IOException {

        String deliveryItemId = "2ae61e82-0af3-518f-b20d-fd2ca06b5cff";
        String pedUrl = "/ProcessEventDirectory?%26$filter=%20(process_id%20eq%20guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff')%20and%20(%20correlationType_code%20eq%20'EARLY_REPORTED'%20or%20correlationType_code%20eq%20'REPORTED'%20or%20correlationType_code%20eq%20'LATE_REPORTED'%20or%20correlationType_code%20eq%20'UNPLANNED'%20or%20correlationType_code%20eq%20'UNPLANNED_DELAYED'%20or%20correlationType_code%20eq%20'UNPLANNED_ONTIME'%20)%20%26$expand=event%20,%20plannedEvent%20%26$orderby=event%2FactualBusinessTimestamp%20";

        String pedJson = IOUtils.toString(new ClassPathResource("/odata/ped-route.json").getInputStream());
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(pedJson, ProcessEventDirectory.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/ProcessEventDirectory"), eq(ProcessEventDirectory.class))).thenReturn(
                processEventDirectoryODataResultList);

        String goodsIssuedJsonUrl = "/GoodsIssued?%26$filter=%20(id%20eq%20guid'cf32a237-e760-11ea-b9d1-df5840db32fa')%20";
        String goodsIssuedJson = IOUtils.toString(new ClassPathResource("/odata/goodsIssued-route.json").getInputStream());
        ODataResultList<EventEx> goodsIssued = ODataUtils.readEntitySet(goodsIssuedJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/GoodsIssued"), eq(EventEx.class))).thenReturn(
                goodsIssued);


        String pickingUrl = "/Picking?%26$filter=%20(id%20eq%20guid'cb2186b0-e760-11ea-b9d1-493ddc8ffb9a')%20";
        String pickingJson = IOUtils.toString(new ClassPathResource("/odata/picking-route.json").getInputStream());
        ODataResultList<EventEx> picking = ODataUtils.readEntitySet(pickingJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/Picking"), eq(EventEx.class))).thenReturn(
                picking);

        String delayUrl = "/Delay?%26$filter=%20(id%20eq%20guid'913570d5-ea85-11ea-b9d1-57ee9073f52e')%20%26$expand=estimatedArrival%20";
        String delayJson = IOUtils.toString(new ClassPathResource("/odata/delay-route.json").getInputStream());
        ODataResultList<EventEx> delay = ODataUtils.readEntitySet(delayJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/Delay"), eq(EventEx.class))).thenReturn(
                delay);

        String plannedEventUrl = "/PlannedEvent?%26$filter=%20(((eventMatchKey%20eq%20'00000020900003')%20and%20(eventType%20eq%20'com.lbngttsamples.gtt.app.sof.Shipment.Arrival'))%20and%20(process_id%20eq%20guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff'))%20";
        String plannedEventJson = IOUtils.toString(new ClassPathResource("/odata/planned-event-route.json").getInputStream());
        ODataResultList<PlannedEvent> plannedEvent = ODataUtils.readEntitySet(plannedEventJson, PlannedEvent.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/PlannedEvent"), eq(PlannedEvent.class))).thenReturn(
                plannedEvent);

        String locationAltKey = "xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:LogisticLocation:NEWYORK";
        String locationJson = IOUtils.toString(new ClassPathResource("/odata/location-route.json").getInputStream());
        ODataResultList<Location> locationODataResultList = ODataUtils.readEntitySet(locationJson, Location.class);
        Mockito.when(gttCoreServiceClient.getLocation(locationAltKey)).thenReturn(locationODataResultList.getResults().get(0));


        String plannedEventJson1 = IOUtils.toString(new ClassPathResource("/odata/plannedEvent-route.json").getInputStream());
        ODataResultList<PlannedEvent> plannedEvent1 = ODataUtils.readEntitySet(plannedEventJson1, PlannedEvent.class);
        Mockito.when(pofService.getPlannedEvents4TP(UUID.fromString(deliveryItemId))).thenReturn(plannedEvent1.getResults());


        String locationsJson = IOUtils.toString(new ClassPathResource("/odata/locations-route.json").getInputStream());
        ODataResultList<Location> locations = ODataUtils.readEntitySet(locationsJson, Location.class);
        Mockito.when(gttCoreServiceClient.getLocations(anySet())).thenReturn(locations.getResults());

        String shipmentUrl = "/Shipment?%26$filter=%20(altKey%20eq%20'xri%3A%2F%2Fsap.com%2Fid%3ALBN%2310010001016%3AQM7CLNT910%3ASHIPMENT_ORDER%3A0000002090')%20%26$expand=stopsForVP%20";
        String shipmentJson = IOUtils.toString(new ClassPathResource("/odata/shipment-route.json").getInputStream());
        ODataResultList<Shipment> shipment = ODataUtils.readEntitySet(shipmentJson, Shipment.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/Shipment"), eq(Shipment.class))).thenReturn(
                shipment);

        String deliveryItemUrl = "/InboundDeliveryItem(guid'2ae61e82-0af3-518f-b20d-fd2ca06b5cff')";
        String deliveryItemJson = IOUtils.toString(new ClassPathResource("/odata/deliveryItem-route.json").getInputStream());
        InboundDeliveryItem deliveryItem = ODataUtils.readEntity(deliveryItemJson,InboundDeliveryItem.class);
        Mockito.when(gttCoreServiceClient.readEntity(contains("/InboundDeliveryItem"), eq(InboundDeliveryItem.class))).thenReturn(
                deliveryItem);

        String locationAltKey1 = "xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:Customer:LBN_CUS_CL";
        String locationJson1 = IOUtils.toString(new ClassPathResource("/odata/location1-route.json").getInputStream());
        ODataResultList<Location> locationODataResultList1 = ODataUtils.readEntitySet(locationJson1, Location.class);
        Mockito.when(gttCoreServiceClient.getLocation(locationAltKey1)).thenReturn(locationODataResultList1.getResults().get(0));

        String getDepartureEventUrl = "/Departure?$filter=(id%20eq%20guid'fb122676-ea82-11ea-b9d1-bda152ca4d8c')%20or%20(id%20eq%20guid'71dd2c10-e763-11ea-b9d1-5ba2a71155f8')";
        String departureEventJson = IOUtils.toString(new ClassPathResource("/odata/departure-event-route.json").getInputStream());
        ODataResultList<EventEx> departureEvent = ODataUtils.readEntitySet(departureEventJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/Departure"), eq(EventEx.class))).thenReturn(
                departureEvent);

        String getArrivalEventUrl = "/Arrival?$filter=id%20eq%20guid'b524a419-e762-11ea-b9d1-e5a04a2b62ad'";
        String arrivalEventJson = IOUtils.toString(new ClassPathResource("/odata/arrival-event-route.json").getInputStream());
        ODataResultList<EventEx> arrivalEvent = ODataUtils.readEntitySet(arrivalEventJson, EventEx.class);
        Mockito.when(gttCoreServiceClient.readEntitySetAll(contains("/Arrival"), eq(EventEx.class))).thenReturn(
                arrivalEvent);

        List<Route> routes = mapService.getRoutes(deliveryItemId);

        Assert.assertEquals(3,routes.size());
    }
}
