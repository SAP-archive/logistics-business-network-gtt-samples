package com.sap.gtt.v2.sample.sof.odata.handler;


import com.google.gson.Gson;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrderItem;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
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

import java.io.IOException;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SOFUtils.class)
public class SOIODataHandlerTest {

    @Mock
    private GTTCoreServiceClient client;
    @Mock
    private SOFService sofService;
    @InjectMocks
    private SOIOdataHandler handler;

    @Before
    public void setUp() {
        mockStatic(SOFUtils.class);
        given(SOFUtils.getGson()).willReturn(new Gson());
    }

    @Test
    public void testReadSalesOrderItemSet() throws IOException {
        String querySalesOrderItemSet = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem?$expand=deliveryItemTPs,deliveryItemTPs/deliveryItem&$format=json";

        String json = IOUtils.toString(new ClassPathResource("/odata/sales-order-items.json").getInputStream());
        ODataResultList<SalesOrderItem> entityList = ODataUtils.readEntitySet(json, SalesOrderItem.class);
        when(this.client.readEntitySet(anyString(), eq(SalesOrderItem.class))).thenReturn(entityList);
        given(SOFUtils.getNormalizedUri(any())).willReturn(querySalesOrderItemSet);

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(null, null);
        Assert.assertEquals(new Integer(3), response.getCount());
        Assert.assertEquals(3, response.getResults().size());
    }

    @Test
    public void testReadSalesOrderItemEntity() throws IOException {
        String querySalesOrderItemEntity = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrderItem(guid'sarahTest')?$expand=deliveryItemTPs,deliveryItemTPs/deliveryItem&$format=json";

        String json = IOUtils.toString(new ClassPathResource("/odata/sales-order-items.json").getInputStream());
        ODataResultList<SalesOrderItem> entityList = ODataUtils.readEntitySet(json, SalesOrderItem.class);
        SalesOrderItem entity = entityList.getResults().get(0);

        when(this.client.readEntity(anyString(), eq(SalesOrderItem.class))).thenReturn(entity);
        given(SOFUtils.getNormalizedUri(any())).willReturn(querySalesOrderItemEntity);
        Map<String, Object> response = this.handler.handleReadEntity(null, null);
        Assert.assertNotNull(response);
    }

}
