package com.sap.gtt.v2.sample.sof.rest.controller.domain.executionflow;

import java.util.List;

public class ExecutionFlow {

    private List<Lane> lanes;
    private List<Node> nodes;

    public List<Lane> getLanes() {
        return lanes;
    }

    public void setLanes(List<Lane> lanes) {
        this.lanes = lanes;
    }

    public List<Node> getNodes() {
        return nodes;
    }

    public void setNodes(List<Node> nodes) {
        this.nodes = nodes;
    }
}
