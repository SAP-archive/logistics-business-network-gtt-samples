package com.sap.gtt.v2.sample.pof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class PurchaseOrderTest {

    private static final String SALES_ORDER_NO = "purchaseOrderNo";
    private static final BigDecimal BIG_DECIMAL_ZERO = new BigDecimal("0.00");
    private static final String CURRENCY = "currency";
    private static final String LOGICAL_SYSTEM = "logicalSystem";
    private static final String INCOTERMS_CODE = "incotermsCode";
    private static final String INCOTERMS_LOCATION = "incotermsLocation";
    private static final String INCOTERMS_VERSION = "incotermsVersion";

    @Test
    public void testSetterAndGetter() {
        PurchaseOrder purchaseOrder = new PurchaseOrder();

        purchaseOrder.setPurchaseOrderNo(SALES_ORDER_NO);
        purchaseOrder.setNetValue(BIG_DECIMAL_ZERO);
        purchaseOrder.setCurrency(CURRENCY);
        purchaseOrder.setLogicalSystem(LOGICAL_SYSTEM);
        purchaseOrder.setIncotermsCode(INCOTERMS_CODE);
        purchaseOrder.setIncotermsLocation(INCOTERMS_LOCATION);
        purchaseOrder.setIncotermsVersion(INCOTERMS_VERSION);
        purchaseOrder.setTotalDelayedValue(BIG_DECIMAL_ZERO);
        purchaseOrder.setCompletionValue(BIG_DECIMAL_ZERO);

        UUID id = UUID.randomUUID();
        purchaseOrder.setId(id);
        Incoterms incoterms = new Incoterms();
        purchaseOrder.setIncoterms(incoterms);
        List<PurchaseOrderItem> tps = new ArrayList<>();
        purchaseOrder.setPurchaseOrderItemTPs(tps);

        Assert.assertEquals(SALES_ORDER_NO, purchaseOrder.getPurchaseOrderNo());
        Assert.assertEquals(BIG_DECIMAL_ZERO, purchaseOrder.getNetValue());
        Assert.assertEquals(CURRENCY, purchaseOrder.getCurrency());
        Assert.assertEquals(LOGICAL_SYSTEM, purchaseOrder.getLogicalSystem());
        Assert.assertEquals(INCOTERMS_CODE, purchaseOrder.getIncotermsCode());
        Assert.assertEquals(INCOTERMS_LOCATION, purchaseOrder.getIncotermsLocation());
        Assert.assertEquals(INCOTERMS_VERSION, purchaseOrder.getIncotermsVersion());
        Assert.assertEquals(BIG_DECIMAL_ZERO, purchaseOrder.getCompletionValue());
        Assert.assertEquals(id, purchaseOrder.getId());
        Assert.assertEquals(incoterms, purchaseOrder.getIncoterms());
        Assert.assertEquals(tps, purchaseOrder.getPurchaseOrderItemTPs());
    }
}
