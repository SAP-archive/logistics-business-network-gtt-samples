package com.sap.gtt.v2.sample.sof.rest.controller.domain.fulfillmentprocessflow;

import java.util.List;

public class FulfillmentProcessFlow {

    private List<Lane> lanes;

    public List<Lane> getLanes() {
        return lanes;
    }

    public void setLanes(List<Lane> lanes) {
        this.lanes = lanes;
    }
}
