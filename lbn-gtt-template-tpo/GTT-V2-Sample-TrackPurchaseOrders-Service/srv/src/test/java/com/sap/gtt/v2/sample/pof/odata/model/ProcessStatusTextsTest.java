package com.sap.gtt.v2.sample.pof.odata.model;

import org.junit.Assert;
import org.junit.Test;

public class ProcessStatusTextsTest {

    private static final String CODE = "code";
    private static final String NAME = "name";
    private static final String LOCALE = "locale";

    @Test
    public void testSetterAndGetter() {
        ProcessStatusTexts text = new ProcessStatusTexts();
        text.setCode(CODE);
        text.setName(NAME);
        text.setLocale(LOCALE);

        Assert.assertEquals(CODE, text.getCode());
        Assert.assertEquals(NAME, text.getName());
        Assert.assertEquals(LOCALE, text.getLocale());

    }

}
