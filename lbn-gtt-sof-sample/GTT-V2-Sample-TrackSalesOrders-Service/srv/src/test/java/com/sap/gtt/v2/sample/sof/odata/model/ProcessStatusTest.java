package com.sap.gtt.v2.sample.sof.odata.model;

import org.junit.Assert;
import org.junit.Test;

public class ProcessStatusTest {

    private static final String CODE = "code";
    private static final String NAME = "name";

    @Test
    public void testSetterAndGetter() {
        ProcessStatus status = new ProcessStatus();
        status.setCode(CODE);
        status.setName(NAME);

        ProcessStatusTexts text = new ProcessStatusTexts();
        status.setLocalized(text);

        Assert.assertEquals(CODE, status.getCode());
        Assert.assertEquals(NAME, status.getName());
        Assert.assertEquals(text, status.getLocalized());

    }

}
