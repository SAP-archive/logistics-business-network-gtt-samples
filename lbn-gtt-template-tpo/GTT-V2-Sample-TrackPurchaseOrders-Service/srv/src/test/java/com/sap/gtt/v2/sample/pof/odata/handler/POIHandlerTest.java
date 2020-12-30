package com.sap.gtt.v2.sample.pof.odata.handler;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.pof.App;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
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
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.io.ClassPathResource;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@SpringBootTest(classes = App.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@PrepareForTest(POFUtils.class)
public class POIHandlerTest {

    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private POFPurchaseOrderItemODataHandler handler;

    @Mock
    private LocationService locationService;

    @Before
    public void setUp() {
        ReflectionTestUtils.setField(handler, "gttCoreServiceClient", client);
        ReflectionTestUtils.setField(handler, "locationService", locationService);

        mockStatic(POFUtils.class);
        given(POFUtils.getGson()).willReturn(new Gson());
    }

    @Test
    public void testReadPurchaseOrderItemSet() throws IOException {
        String querySalesOrderSet = "sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem?$expand=receivingLocationType," +
                "supplierLocationType, toReceivingLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);
        when(this.client.readEntitySet(anyString(), eq(PurchaseOrderItem.class))).thenReturn(entityList);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderSet);

        String jsonProcessEvent = IOUtils.toString(new ClassPathResource("/odata/process-event-directory.json").getInputStream());
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(jsonProcessEvent, ProcessEventDirectory.class);
        when(this.client.readEntitySetAll(anyString(), eq(ProcessEventDirectory.class))).thenReturn(processEventDirectoryODataResultList);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderSet);
        given(POFUtils.removeFieldFromUrl("sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem?$expand=receivingLocationType,supplierLocationType, toReceivingLocation, toSupplierLocation",
                "toReceivingLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem?$expand=receivingLocationType,supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem?$expand=receivingLocationType,supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem?$expand=receivingLocationType,supplierLocationType");

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(null, null);
        Assert.assertEquals(2, response.getResults().size());
    }

    @Test
    public void testReadPurchaseOrderItemEntity() throws IOException {
        String querySalesOrderEntity = "/sap/logistics/gtt/sample/pof/odata/v1" +
                "/PurchaseOrderItem(guid'e34ed557-4904-51bc-bd43-d954b2b478a8')?$format=json" +
                "&$expand=receivingLocationType,supplierLocationType, toReceivingLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);
        PurchaseOrderItem entity = entityList.getResults().get(0);

        when(this.client.readEntity(anyString(), eq(PurchaseOrderItem.class))).thenReturn(entity);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderEntity);

        String jsonProcessEvent = IOUtils.toString(new ClassPathResource("/odata/process-event-directory.json").getInputStream());
        ODataResultList<ProcessEventDirectory> processEventDirectoryODataResultList = ODataUtils.readEntitySet(jsonProcessEvent, ProcessEventDirectory.class);
        when(this.client.readEntitySetAll(anyString(), eq(ProcessEventDirectory.class))).thenReturn(processEventDirectoryODataResultList);

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem(guid'e34ed557-4904-51bc-bd43-d954b2b478a8')?$format=json" +
                        "&$expand=receivingLocationType,supplierLocationType, toReceivingLocation, toSupplierLocation",
                "toReceivingLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem(guid'e34ed557-4904-51bc-bd43-d954b2b478a8')?$format=json" +
                "&$expand=receivingLocationType,supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem(guid'e34ed557-4904-51bc-bd43-d954b2b478a8')?$format=json" +
                        "&$expand=receivingLocationType,supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("/sap/logistics/gtt/sample/pof/odata/v1/PurchaseOrderItem(guid'e34ed557-4904-51bc-bd43-d954b2b478a8')?$format=json" +
                "&$expand=receivingLocationType,supplierLocationType");

        Map<String, Object> response = this.handler.handleReadEntity(null, null);

        Assert.assertNotNull(response);
    }
}
