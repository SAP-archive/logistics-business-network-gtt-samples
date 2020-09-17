package com.sap.gtt.v2.sample.sof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.util.UUID;

public class SalesOrderItemTPTest {

    private static final String SALES_ORDER_ITEM_ALT_KEY = "salesOrderItemAltKey";

    @Test
    public void testSetterAndGetter() {
        SalesOrderItemTP salesOrderItemTP = new SalesOrderItemTP();
        salesOrderItemTP.setSalesOrderItemAltKey(SALES_ORDER_ITEM_ALT_KEY);

        UUID id = UUID.randomUUID();
        salesOrderItemTP.setSalesOrderId(id);
        salesOrderItemTP.setSalesOrderItemId(id);

        SalesOrderItem item = new SalesOrderItem();
        salesOrderItemTP.setSalesOrderItem(item);

        Assert.assertEquals(SALES_ORDER_ITEM_ALT_KEY, salesOrderItemTP.getSalesOrderItemAltKey());
        Assert.assertEquals(id, salesOrderItemTP.getSalesOrderId());
        Assert.assertEquals(id, salesOrderItemTP.getSalesOrderItemId());
        Assert.assertEquals(item, salesOrderItemTP.getSalesOrderItem());

    }

}
