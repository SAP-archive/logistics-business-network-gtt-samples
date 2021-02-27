package com.sap.gtt.v2.sample.pof.rest.domain.documentflow;

import org.junit.Test;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.junit.Assert.assertEquals;

public class AttributeTest {

    @Test
    public void attributeTest() {
        Attribute attribute = new Attribute(EMPTY, EMPTY, EMPTY, EMPTY, 0);

        attribute.setPropertyName(EMPTY);
        attribute.setUom(EMPTY);
        attribute.setValue(EMPTY);
        attribute.setValueStatus(EMPTY);
        attribute.setGroup(0);

        assertEquals(EMPTY, attribute.getPropertyName());
        assertEquals(EMPTY, attribute.getUom());
        assertEquals(EMPTY, attribute.getValue());
        assertEquals(EMPTY, attribute.getValueStatus());
        assertEquals(0, attribute.getGroup().intValue());
    }

}