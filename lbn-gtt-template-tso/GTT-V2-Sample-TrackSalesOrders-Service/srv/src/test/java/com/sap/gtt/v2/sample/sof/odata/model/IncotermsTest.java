package com.sap.gtt.v2.sample.sof.odata.model;

import org.junit.Assert;
import org.junit.Test;

import java.util.UUID;

public class IncotermsTest {

    private static final String CODE = "code";
    private static final String NAME = "name";

    @Test
    public void testSetterAndGetter() {
        Incoterms incoterms = new Incoterms();
        incoterms.setCode(CODE);
        incoterms.setName(NAME);

        Assert.assertEquals(CODE, incoterms.getCode());
        Assert.assertEquals(NAME, incoterms.getName());

    }

}
