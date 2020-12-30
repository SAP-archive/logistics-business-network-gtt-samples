package com.sap.gtt.v2.sample.pof.rest.domain.documentflow;

import java.util.List;

public class DocumentFlow {

    private List<Node> nodes;
    private List<Line> lines;
    private List<Group> groups;

    public List<Node> getNodes() {
        return nodes;
    }

    public void setNodes(List<Node> nodes) {
        this.nodes = nodes;
    }

    public List<Line> getLines() {
        return lines;
    }

    public void setLines(List<Line> lines) {
        this.lines = lines;
    }

    public List<Group> getGroups() {
        return groups;
    }

    public void setGroups(List<Group> groups) {
        this.groups = groups;
    }

}
