package com.sap.gtt.v2.sample.pof.odata.handler;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.service.LocationService;
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
import java.nio.charset.StandardCharsets;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class PurchaseOrderHandlerTest {
    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private POFPurchaseOrderODataHandler handler;

    @Mock
    private LocationService locationService;

    @Mock
    private POFPurchaseOrderItemODataHandler itemODataHandler;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(handler, "gttCoreServiceClient", client);
        ReflectionTestUtils.setField(handler, "locationService", locationService);
        ReflectionTestUtils.setField(handler, "purchaseOrderItemODataHandler", itemODataHandler);

        mockStatic(POFUtils.class);
        given(POFUtils.getGson()).willReturn(new Gson());
    }

    @Test
    public void testReadPurchaseOrderSet() throws IOException {
        String querySalesOrderSet = "/sap/logistics/gtt/sample/pof/odata/v1" +
                "/PurchaseOrder?$format=json&$inlinecount=allpages" +
                "&$expand=purchaseOrderItemTPs, purchasesOrderItemTPs/purchaseOrderItem, incoterms,receivingLocationType, " +
                "supplierLocationType, toReceivingLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-orders.json").getInputStream(), StandardCharsets.UTF_8);
        ODataResultList<PurchaseOrder> entityList = ODataUtils.readEntitySet(json, PurchaseOrder.class);
        when(this.client.readEntitySet(anyString(), eq(PurchaseOrder.class))).thenReturn(entityList);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderSet);

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder?$format=json&$inlinecount=allpages&$expand=purchaseOrderItemTPs, purchasesOrderItemTPs/purchaseOrderItem, " +
                        "incoterms,receivingLocationType, supplierLocationType, toReceivingLocation, toSupplierLocation",
                "toReceivingLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder?$format=json&$inlinecount=allpages&$expand=purchaseOrderItemTPs, purchasesOrderItemTPs/purchaseOrderItem, " +
        "incoterms,receivingLocationType, supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder?$format=json&$inlinecount=allpages&$expand=purchaseOrderItemTPs, " +
                        "purchasesOrderItemTPs/purchaseOrderItem, incoterms,receivingLocationType, supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder?$format=json&$inlinecount=allpages&$expand=purchaseOrderItemTPs," +
                " purchasesOrderItemTPs/purchaseOrderItem, incoterms,receivingLocationType, supplierLocationType");

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(null, null);
        Assert.assertEquals(2, response.getResults().size());
    }

    @Test
    public void testReadPurchaseOrderEntity() throws IOException {
        String querySalesOrderEntity = "/sap/logistics/gtt/sample/pof/odata/v1" +
                "/PurchaseOrder(guid'ffde2cf6-e6f7-5383-b9f7-b6db96f9ab5f')?$format=json" +
                "&$expand=purchaseOrderItemTPs, purchaseOrderItemTPs/purchaseOrderItem, incoterms, receivingLocationType, " +
                "supplierLocationType, toReceivingLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order.json").getInputStream(), StandardCharsets.UTF_8);
        PurchaseOrder entity = ODataUtils.readEntity(json, PurchaseOrder.class);

        when(this.client.readEntity(anyString(), eq(PurchaseOrder.class))).thenReturn(entity);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderEntity);
        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder(guid'ffde2cf6-e6f7-5383-b9f7-b6db96f9ab5f')?$format=json" +
                        "&$expand=purchaseOrderItemTPs, purchaseOrderItemTPs/purchaseOrderItem, incoterms, receivingLocationType, supplierLocationType, toReceivingLocation, toSupplierLocation",
                "toReceivingLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder(guid'ffde2cf6-e6f7-5383-b9f7-b6db96f9ab5f')?$format=json" +
                "&$expand=purchaseOrderItemTPs, purchaseOrderItemTPs/purchaseOrderItem, incoterms, receivingLocationType, supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder(guid'ffde2cf6-e6f7-5383-b9f7-b6db96f9ab5f')?$format=json&$expand=purchaseOrderItemTPs, " +
                        "purchaseOrderItemTPs/purchaseOrderItem, incoterms, receivingLocationType, supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrder(guid'ffde2cf6-e6f7-5383-b9f7-b6db96f9ab5f')?$format=json&$expand=purchaseOrderItemTPs, " +
                "purchaseOrderItemTPs/purchaseOrderItem, incoterms, receivingLocationType, supplierLocationType");

        Map<String, Object> response = this.handler.handleReadEntity(null, null);

        Assert.assertNotNull(response);
    }
}
