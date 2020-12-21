package com.sap.gtt.v2.sample.pof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.UUID;

public class PurchaseOrderItemTest {

    private static final String SALES_ORDER_NO = "salesOrderNo";
    private static final String ITEM_NO = "itemNo";
    private static final String MATERIAL_DESCRIPTION = "materialDescription";
    private static final String PROCESS_STATUS_CODE = "processStatus_code";
    private static final String CURRENCY = "currency";
    private static final BigDecimal BIG_DECIMAL_ZERO = new BigDecimal("0.00");

    @Test
    public void testSetterAndGetter() {
        PurchaseOrderItem purchaseOrderItem = new PurchaseOrderItem();

        purchaseOrderItem.setPurchaseOrderNo(SALES_ORDER_NO);
        purchaseOrderItem.setItemNo(ITEM_NO);
        purchaseOrderItem.setMaterialDescription(MATERIAL_DESCRIPTION);
        purchaseOrderItem.setProcessStatusCode(PROCESS_STATUS_CODE);
        purchaseOrderItem.setCurrency(CURRENCY);
        purchaseOrderItem.setNetValue(BIG_DECIMAL_ZERO);
        purchaseOrderItem.setOrderQuantity(BIG_DECIMAL_ZERO);

        UUID id = UUID.randomUUID();
        purchaseOrderItem.setId(id);
        ProcessStatus status = new ProcessStatus();
        purchaseOrderItem.setProcessStatus(status);

        Assert.assertEquals(id, purchaseOrderItem.getId());
        Assert.assertEquals(ITEM_NO, purchaseOrderItem.getItemNo());
        Assert.assertEquals(SALES_ORDER_NO, purchaseOrderItem.getPurchaseOrderNo());
        Assert.assertEquals(MATERIAL_DESCRIPTION, purchaseOrderItem.getMaterialDescription());
        Assert.assertEquals(BIG_DECIMAL_ZERO, purchaseOrderItem.getOrderQuantity());
        Assert.assertEquals(BIG_DECIMAL_ZERO, purchaseOrderItem.getNetValue());
        Assert.assertEquals(status, purchaseOrderItem.getProcessStatus());
        Assert.assertEquals(PROCESS_STATUS_CODE, purchaseOrderItem.getProcessStatusCode());
        Assert.assertEquals(CURRENCY, purchaseOrderItem.getCurrency());
    }

}
