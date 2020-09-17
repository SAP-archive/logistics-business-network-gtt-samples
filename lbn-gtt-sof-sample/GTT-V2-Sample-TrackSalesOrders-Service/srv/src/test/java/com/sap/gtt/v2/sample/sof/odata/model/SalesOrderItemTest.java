package com.sap.gtt.v2.sample.sof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.UUID;

public class SalesOrderItemTest {

    private static final String SALES_ORDER_NO = "salesOrderNo";
    private static final String ITEM_NO = "itemNo";
    private static final String MATERIAL_NO = "materialNo";
    private static final String MATERIAL_DESCRIPTION = "materialDescription";
    private static final String PLANT = "plant";
    private static final String UOM = "uom";
    private static final String PROCESS_STATUS_CODE = "processStatus_code";
    private static final String CURRENCY = "currency";
    private static final BigDecimal BIG_DECIMAL_ZERO = new BigDecimal("0.00");

    @Test
    public void testSetterAndGetter() {
        SalesOrderItem salesOrderItem = new SalesOrderItem();

        salesOrderItem.setSalesOrderNo(SALES_ORDER_NO);
        salesOrderItem.setItemNo(ITEM_NO);
        salesOrderItem.setMaterialNo(MATERIAL_NO);
        salesOrderItem.setMaterialDescription(MATERIAL_DESCRIPTION);
        salesOrderItem.setPlant(PLANT);
        salesOrderItem.setUom(UOM);
        salesOrderItem.setProcessStatusCode(PROCESS_STATUS_CODE);
        salesOrderItem.setCurrency(CURRENCY);
        salesOrderItem.setCompletionQuantity(BIG_DECIMAL_ZERO);
        salesOrderItem.setDelayedQuantity(BIG_DECIMAL_ZERO);
        salesOrderItem.setNetValue(BIG_DECIMAL_ZERO);
        salesOrderItem.setOrderQuantity(BIG_DECIMAL_ZERO);

        UUID id = UUID.randomUUID();
        salesOrderItem.setId(id);
        ProcessStatus status = new ProcessStatus();
        salesOrderItem.setProcessStatus(status);

        Assert.assertEquals(id, salesOrderItem.getId());
        Assert.assertEquals(ITEM_NO, salesOrderItem.getItemNo());
        Assert.assertEquals(SALES_ORDER_NO, salesOrderItem.getSalesOrderNo());
        Assert.assertEquals(MATERIAL_NO, salesOrderItem.getMaterialNo());
        Assert.assertEquals(MATERIAL_DESCRIPTION, salesOrderItem.getMaterialDescription());
        Assert.assertEquals(PLANT, salesOrderItem.getPlant());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrderItem.getOrderQuantity());
        Assert.assertEquals(UOM, salesOrderItem.getUom());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrderItem.getNetValue());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrderItem.getDelayedQuantity());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrderItem.getCompletionQuantity());
        Assert.assertEquals(status, salesOrderItem.getProcessStatus());
        Assert.assertEquals(PROCESS_STATUS_CODE, salesOrderItem.getProcessStatusCode());
        Assert.assertEquals(CURRENCY, salesOrderItem.getCurrency());

    }

}
