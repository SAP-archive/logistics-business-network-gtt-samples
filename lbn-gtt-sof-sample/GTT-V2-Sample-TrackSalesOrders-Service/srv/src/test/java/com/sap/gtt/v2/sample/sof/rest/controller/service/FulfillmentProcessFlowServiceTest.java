package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrderItem;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.fulfillmentprocessflow.FulfillmentProcessFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.fulfillmentprocessflow.Lane;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;

@RunWith(PowerMockRunner.class)
@PrepareForTest(SOFUtils.class)
public class FulfillmentProcessFlowServiceTest {

    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private FulfillmentProcessFlowService fulfillmentProcessFlowService;

    @Test
    public void testGenerateFulfillmentProcessFlowWhenPartReject() {
        String partRejectJson = SOFUtils.getStringFromResource("/odata/fulfillment-process-flow-part-reject.json");
        String goodsIssueJson = SOFUtils.getStringFromResource("/odata/fulfillment-process-flow-goods-issue.json");
        Mockito.when(client.readEntity(contains("/SalesOrderItem(guid'7a9cd038-0e53-509d-a75a-c2cb6801d1f8')"), eq(SalesOrderItem.class))).thenReturn(ODataUtils.readEntity(partRejectJson, SalesOrderItem.class));
        Mockito.when(client.readEntitySet(contains("/ProcessEventDirectory?$expand=process"), eq(ProcessEventDirectory.class))).thenReturn(ODataUtils.readEntitySet(goodsIssueJson, ProcessEventDirectory.class));

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(UUID.fromString("7a9cd038-0e53-509d-a75a-c2cb6801d1f8"));
        List<Lane> lanes =  flow.getLanes();
        Lane lane1 = lanes.get(0);
        Lane lane2 = lanes.get(1);
        Lane lane3 = lanes.get(3);

        Assert.assertEquals(5, lanes.size());

        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CREATED.getName(), lane1.getName());
        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CREATED.getPosition(), lane1.getPosition());
        Assert.assertEquals(new BigDecimal("11.000"), lane1.getCount());
        Assert.assertEquals(new BigDecimal("11.000"), lane1.getTotal());

        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CONFIRMED.getName(), lane2.getName());
        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CONFIRMED.getPosition(), lane2.getPosition());
        Assert.assertEquals(new BigDecimal("5.000"), lane2.getCount());
        Assert.assertEquals(new BigDecimal("6.000"), lane2.getRejectCount());
        Assert.assertEquals(new BigDecimal("11.000"), lane2.getTotal());

        Assert.assertEquals(FulfillmentProcessMilestoneEnum.DELIVERY_GOODS_ISSUED.getName(), lane3.getName());
        Assert.assertEquals(FulfillmentProcessMilestoneEnum.DELIVERY_GOODS_ISSUED.getPosition(), lane3.getPosition());
        Assert.assertEquals(new BigDecimal("5.000"), lane3.getCount());
        Assert.assertEquals(new BigDecimal("5.000"), lane3.getTotal());

    }

    @Test
    public void testGenerateFulfillmentProcessFlowWhenAllReject() {
        String partRejectJson = SOFUtils.getStringFromResource("/odata/fulfillment-process-flow-all-reject.json");
        Mockito.when(client.readEntity(contains("/SalesOrderItem(guid'106d23ab-9ee3-5931-9d29-9acdddbf6bcc')"), eq(SalesOrderItem.class))).thenReturn(ODataUtils.readEntity(partRejectJson, SalesOrderItem.class));

        FulfillmentProcessFlow flow = fulfillmentProcessFlowService.generateFulfillmentProcessFlow(UUID.fromString("106d23ab-9ee3-5931-9d29-9acdddbf6bcc"));
        List<Lane> lanes =  flow.getLanes();
        Lane lane1 = lanes.get(0);
        Lane lane2 = lanes.get(1);

        Assert.assertEquals(2, lanes.size());

        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CREATED.getName(), lane1.getName());
        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_CREATED.getPosition(), lane1.getPosition());
        Assert.assertEquals(new BigDecimal("10.000"), lane1.getCount());
        Assert.assertEquals(new BigDecimal("10.000"), lane1.getTotal());

        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_REJECTED.getName(), lane2.getName());
        Assert.assertEquals(FulfillmentProcessMilestoneEnum.SALES_ORDER_ITEM_REJECTED.getPosition(), lane2.getPosition());
        Assert.assertEquals(new BigDecimal("10.000"), lane2.getCount());
        Assert.assertEquals(new BigDecimal("10.000"), lane2.getTotal());
    }

}
