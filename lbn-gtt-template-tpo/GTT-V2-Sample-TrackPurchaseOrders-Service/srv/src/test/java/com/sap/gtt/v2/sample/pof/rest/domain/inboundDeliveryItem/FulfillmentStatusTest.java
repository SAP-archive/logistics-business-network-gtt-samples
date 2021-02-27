package com.sap.gtt.v2.sample.pof.rest.domain.inboundDeliveryItem;

import org.junit.Test;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.junit.Assert.assertEquals;

public class FulfillmentStatusTest {

    @Test
    public void fulfillmentStatusTest() {
        FulfillmentStatus fulfillmentStatus = new FulfillmentStatus(EMPTY, 0);

        fulfillmentStatus.setEventStatusCode(EMPTY);
        fulfillmentStatus.setCount(0);

        assertEquals(EMPTY, fulfillmentStatus.getEventStatusCode());
        assertEquals(0, fulfillmentStatus.getCount().intValue());
    }
}