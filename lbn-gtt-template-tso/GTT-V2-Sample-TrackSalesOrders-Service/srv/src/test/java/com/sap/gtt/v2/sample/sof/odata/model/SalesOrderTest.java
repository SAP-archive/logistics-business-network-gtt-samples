package com.sap.gtt.v2.sample.sof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class SalesOrderTest {

    private static final String SALES_ORDER_NO = "salesOrderNo";
    private static final String SHIP_TO_PARTY_ID = "shipToPartyId";
    private static final BigDecimal BIG_DECIMAL_ZERO = new BigDecimal("0.00");
    private static final String CURRENCY = "currency";
    private static final String LOGICAL_SYSTEM = "logicalSystem";
    private static final String CUSTOMER_REFERENCE = "customerReference";
    private static final Long TIME = 0L;
    private static final String INCOTERMS_CODE = "incotermsCode";
    private static final String INCOTERMS_LOCATION = "incotermsLocation";
    private static final String INCOTERMS_VERSION = "incotermsVersion";
    private static final String VIRTUAL_MATERIAL_NO = "vMaterialNo";
    private static final String VIRTUAL_MATERIAL_DESCRIPTION = "vMaterialDescription";

    @Test
    public void testSetterAndGetter() {
        SalesOrder salesOrder = new SalesOrder();

        salesOrder.setSalesOrderNo(SALES_ORDER_NO);
        salesOrder.setShipToPartyId(SHIP_TO_PARTY_ID);
        salesOrder.setNetValue(BIG_DECIMAL_ZERO);
        salesOrder.setCurrency(CURRENCY);
        salesOrder.setLogicalSystem(LOGICAL_SYSTEM);
        salesOrder.setCustomerReference(CUSTOMER_REFERENCE);
        salesOrder.setDocumentDate(TIME);
        salesOrder.setIncotermsCode(INCOTERMS_CODE);
        salesOrder.setIncotermsLocation(INCOTERMS_LOCATION);
        salesOrder.setIncotermsVersion(INCOTERMS_VERSION);
        salesOrder.setDelayedValue(BIG_DECIMAL_ZERO);
        salesOrder.setCompletionValue(BIG_DECIMAL_ZERO);
        salesOrder.setLastChangeDateTime(TIME);
        salesOrder.setvMaterialNo(VIRTUAL_MATERIAL_NO);
        salesOrder.setvMaterialDescription(VIRTUAL_MATERIAL_DESCRIPTION);

        UUID id = UUID.randomUUID();
        salesOrder.setId(id);
        Incoterms incoterms = new Incoterms();
        salesOrder.setIncoterms(incoterms);
        List<SalesOrderItem> tps = new ArrayList<>();
        salesOrder.setSalesOrderItems(tps);

        Assert.assertEquals(SALES_ORDER_NO, salesOrder.getSalesOrderNo());
        Assert.assertEquals(SHIP_TO_PARTY_ID, salesOrder.getShipToPartyId());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrder.getNetValue());
        Assert.assertEquals(CURRENCY, salesOrder.getCurrency());
        Assert.assertEquals(LOGICAL_SYSTEM, salesOrder.getLogicalSystem());
        Assert.assertEquals(CUSTOMER_REFERENCE, salesOrder.getCustomerReference());
        Assert.assertEquals(TIME, salesOrder.getDocumentDate());
        Assert.assertEquals(INCOTERMS_CODE, salesOrder.getIncotermsCode());
        Assert.assertEquals(INCOTERMS_LOCATION, salesOrder.getIncotermsLocation());
        Assert.assertEquals(INCOTERMS_VERSION, salesOrder.getIncotermsVersion());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrder.getDelayedValue());
        Assert.assertEquals(BIG_DECIMAL_ZERO, salesOrder.getCompletionValue());
        Assert.assertEquals(TIME, salesOrder.getLastChangeDateTime());
        Assert.assertEquals(VIRTUAL_MATERIAL_NO, salesOrder.getvMaterialNo());
        Assert.assertEquals(VIRTUAL_MATERIAL_DESCRIPTION, salesOrder.getvMaterialDescription());
        Assert.assertEquals(id, salesOrder.getId());
        Assert.assertEquals(incoterms, salesOrder.getIncoterms());
        Assert.assertEquals(tps, salesOrder.getSalesOrderItems());

    }
}
