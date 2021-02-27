package com.sap.gtt.v2.sample.sof.odata.handler;

import com.google.gson.Gson;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
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
public class SalesOrderODataHandlerTest {

    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private SalesOrderODataHandler handler;

    @Before
    public void setUp() {
        mockStatic(SOFUtils.class);
        given(SOFUtils.getGson()).willReturn(new Gson());
    }

    @Test
    public void testReadSalesOrderSet() throws IOException {
        String querySalesOrderSet = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder?$format=json&$inlinecount=allpages" +
                "&$expand=salesOrderItems, incoterms";

        String json = IOUtils.toString(new ClassPathResource("/odata/sales-orders.json").getInputStream());
        ODataResultList<SalesOrder> entityList = ODataUtils.readEntitySet(json, SalesOrder.class);
        when(this.client.readEntitySet(anyString(), eq(SalesOrder.class))).thenReturn(entityList);
        given(SOFUtils.getNormalizedUri(any())).willReturn(querySalesOrderSet);

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(null, null);
        Assert.assertEquals(new Integer(5), response.getCount());
        Assert.assertEquals(2, response.getResults().size());

    }

    @Test
    public void testReadSalesOrderEntity() throws IOException {
        String querySalesOrderEntity = "/sap/logistics/gtt/sample/sof/odata/v1" +
                "/SalesOrder(guid'to-be-replaced')?$format=json" +
                "&$expand=salesOrderItems, incoterms";

        String json = IOUtils.toString(new ClassPathResource("/odata/sales-orders.json").getInputStream());
        ODataResultList<SalesOrder> entityList = ODataUtils.readEntitySet(json, SalesOrder.class);
        SalesOrder entity = entityList.getResults().get(0);

        when(this.client.readEntity(anyString(), eq(SalesOrder.class))).thenReturn(entity);
        given(SOFUtils.getNormalizedUri(any())).willReturn(querySalesOrderEntity);

        Map<String, Object> response = this.handler.handleReadEntity(null, null);

        Assert.assertNotNull(response);
    }

}
