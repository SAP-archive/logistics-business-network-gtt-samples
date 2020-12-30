package com.sap.gtt.v2.sample.pof.odata.handler;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
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
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class InboundDeliveryHandlerTest {
    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private POFInboundDeliveryItemODataHandler handler;

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
    public void testReadInboundDeliveryItemSet() throws IOException {
        String querySalesOrderSet = "sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem?$expand=plantLocationType," +
                "supplierLocationType, toPlantLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/inbound-delivery-item.json").getInputStream());
        ODataResultList<InboundDeliveryItem> entityList = ODataUtils.readEntitySet(json, InboundDeliveryItem.class);
        when(this.client.readEntitySet(anyString(), eq(InboundDeliveryItem.class))).thenReturn(entityList);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderSet);
        given(POFUtils.removeFieldFromUrl("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem?$expand=plantLocationType,supplierLocationType, toPlantLocation, toSupplierLocation",
                "toPlantLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem?$expand=plantLocationType,supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem?$expand=plantLocationType,supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem?$expand=plantLocationType,supplierLocationType");

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(null, null);
        Assert.assertEquals(2, response.getResults().size());
    }

    @Test
    public void testReadInboundDeliveryEntity() throws IOException {
        String querySalesOrderEntity = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/InboundDeliveryItem(guid'a6c4f8d8-08ed-5dd9-be8f-2acce04b7114')?$format=json" +
                "&$expand=plantLocationType,supplierLocationType, toPlantLocation, toSupplierLocation";

        String json = IOUtils.toString(new ClassPathResource("/odata/inbound-delivery-item.json").getInputStream());
        ODataResultList<InboundDeliveryItem> entityList = ODataUtils.readEntitySet(json, InboundDeliveryItem.class);
        InboundDeliveryItem entity = entityList.getResults().get(0);

        when(this.client.readEntity(anyString(), eq(InboundDeliveryItem.class))).thenReturn(entity);
        given(POFUtils.getNormalizedUri(any())).willReturn(querySalesOrderEntity);

        given(POFUtils.removeFieldFromUrl("/sap/logistics/gtt/sample/sof/odata/v1/InboundDeliveryItem(guid'a6c4f8d8-08ed-5dd9-be8f-2acce04b7114')?$format=json&$expand=plantLocationType,supplierLocationType, toPlantLocation, toSupplierLocation",
                "toPlantLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem(guid'a6c4f8d8-08ed-5dd9-be8f-2acce04b7114')?$expand=plantLocationType,supplierLocationType, toSupplierLocation");

        given(POFUtils.removeFieldFromUrl("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem(guid'a6c4f8d8-08ed-5dd9-be8f-2acce04b7114')?$expand=plantLocationType,supplierLocationType, toSupplierLocation",
                "toSupplierLocation")).willReturn("sap/logistics/gtt/sample/pof/odata/v1/InboundDeliveryItem(guid'a6c4f8d8-08ed-5dd9-be8f-2acce04b7114')?$expand=plantLocationType,supplierLocationType");


        Map<String, Object> response = this.handler.handleReadEntity(null, null);

        Assert.assertNotNull(response);
    }
}
