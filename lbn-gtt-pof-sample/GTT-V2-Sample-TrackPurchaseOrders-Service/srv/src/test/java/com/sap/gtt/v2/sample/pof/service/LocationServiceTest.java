package com.sap.gtt.v2.sample.pof.service;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.pof.odata.handler.POFLocationODataHandler;
import com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderItemODataHandler;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.io.IOUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.BDDMockito.given;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class LocationServiceTest {
    @Mock
    private GTTCoreServiceClient client;
    @InjectMocks
    private LocationService locationService;
    @Mock
    private POFPurchaseOrderItemODataHandler handler;

    @Mock
    private POFLocationODataHandler pofLocationODataHandler;

    @Mock
    private MapService mapService;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(handler, "gttCoreServiceClient", client);
        ReflectionTestUtils.setField(locationService, "pofLocationODataHandler", pofLocationODataHandler);
        ReflectionTestUtils.setField(locationService, "mapService", mapService);

        mockStatic(POFUtils.class);
        given(POFUtils.getGson()).willReturn(new Gson());
    }

    @Test
    public void getLocationsPurchaseOrderItemGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);

        Map<String, LocationDTO> locationDTOMap = locationService.getLocationsForPurchaseOrderItems(entityList.getResults());
        Assert.assertNotNull(locationDTOMap);
    }

    @Test
    public void getLocationsInboundDeliveryItemGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/inbound-delivery-item.json").getInputStream());
        ODataResultList<InboundDeliveryItem> entityList = ODataUtils.readEntitySet(json, InboundDeliveryItem.class);

        Map<String, LocationDTO> locationDTOMap = locationService.getLocationsForInboundDeliveryItem(entityList.getResults());
        Assert.assertNotNull(locationDTOMap);
    }

    @Test
    public void getLocationsPurchaseOrderGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-orders.json").getInputStream());
        ODataResultList<PurchaseOrder> entityList = ODataUtils.readEntitySet(json, PurchaseOrder.class);

        Map<String, LocationDTO> locationDTOMap = locationService.getLocationsForPurchaseOrders(entityList.getResults());
        Assert.assertNotNull(locationDTOMap);
    }

    @Test
    public void getLocationPurchaseOrderGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/sales-order-items.json").getInputStream());
        ODataResultList<PurchaseOrder> entityList = ODataUtils.readEntitySet(json, PurchaseOrder.class);

        locationService.setLocationsForPurchaseOrder(entityList.getResults().get(0), new HashMap<>());
        Assert.assertNotNull(entityList.getResults());
    }

    @Test
    public void getLocationsPurchaseOrderTPsGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order.json").getInputStream());
        PurchaseOrder purchaseOrder = ODataUtils.readEntity(json, PurchaseOrder.class);

        Map<String, LocationDTO> locations = locationService.getLocationsForPurchaseOrderTP(purchaseOrder.getPurchaseOrderItemTPs());
        Assert.assertNotNull(locations);
    }

    @Test
    public void setLocationsPurchaseOrder() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order.json").getInputStream());
        PurchaseOrder purchaseOrder = ODataUtils.readEntity(json, PurchaseOrder.class);

        locationService.setReceivingLocation(purchaseOrder);
        locationService.setSupplierLocation(purchaseOrder);

        Assert.assertNotNull(purchaseOrder);
    }

    @Test
    public void setLocationsPurchaseOrderItemGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);

        locationService.setLocationsForPurchaseOrderItem(entityList.getResults().get(0), new HashMap<>());
        Assert.assertNotNull(entityList.getResults());
    }

    @Test
    public void getLocationsPurchaseOrderItemInboundDeliveryItemGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);

        locationService.getLocationsForPurchaseOrderItemInboundDeliveryItem(entityList.getResults()
                .get(0).getInboundDeliveryItems());
        Assert.assertNotNull(entityList.getResults());
    }


    @Test
    public void setLocationsInboundDeliveryItem() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/inbound-delivery-item.json").getInputStream());
        ODataResultList<InboundDeliveryItem> entityList = ODataUtils.readEntitySet(json, InboundDeliveryItem.class);

        locationService.setLocationsForInboundDelivery(entityList.getResults().get(0), new HashMap<>());
        Assert.assertNotNull(entityList.getResults());
    }

    @Test
    public void setLocationsPurchaseOrderGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrder> entityList = ODataUtils.readEntitySet(json, PurchaseOrder.class);

        locationService.setLocationsForPurchaseOrder(entityList.getResults().get(0), new HashMap<>());
        Assert.assertNotNull(entityList.getResults());
    }

    @Test
    public void setLocationPurchaseOrderItemGet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);

        locationService.setReceivingLocation(entityList.getResults().get(0));
        locationService.setSupplierLocation(entityList.getResults().get(0));
        Assert.assertNotNull(entityList.getResults());
    }

    @Test
    public void setLocationInboundDeliveryItem() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/inbound-delivery-item.json").getInputStream());
        ODataResultList<InboundDeliveryItem> entityList = ODataUtils.readEntitySet(json, InboundDeliveryItem.class);

        locationService.setPlantLocation(entityList.getResults().get(0));
        locationService.setSupplierLocation(entityList.getResults().get(0));
        Assert.assertNotNull(entityList.getResults());
    }
}
