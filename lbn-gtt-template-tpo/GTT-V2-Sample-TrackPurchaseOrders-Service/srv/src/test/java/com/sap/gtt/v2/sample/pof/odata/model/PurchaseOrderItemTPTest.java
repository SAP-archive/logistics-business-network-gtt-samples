package com.sap.gtt.v2.sample.pof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.util.UUID;

public class PurchaseOrderItemTPTest {

    private static final String SALES_ORDER_ITEM_ALT_KEY = "purchaseOrderItemAltKey";

    @Test
    public void testSetterAndGetter() {
        PurchaseOrderItemTP purchaseOrderItemTP = new PurchaseOrderItemTP();
        purchaseOrderItemTP.setPurchaseOrderItemAltKey(SALES_ORDER_ITEM_ALT_KEY);

        UUID id = UUID.randomUUID();
        purchaseOrderItemTP.setPurchaseOrderId(id);
        purchaseOrderItemTP.setPurchaseOrderItemId(id);

        PurchaseOrderItem item = new PurchaseOrderItem();
        purchaseOrderItemTP.setPurchaseOrderItem(item);

        Assert.assertEquals(SALES_ORDER_ITEM_ALT_KEY, purchaseOrderItemTP.getPurchaseOrderItemAltKey());
        Assert.assertEquals(id, purchaseOrderItemTP.getPurchaseOrderId());
        Assert.assertEquals(id, purchaseOrderItemTP.getPurchaseOrderItemId());
        Assert.assertEquals(item, purchaseOrderItemTP.getPurchaseOrderItem());

    }

}
