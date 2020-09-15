package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGeneralStatusEnum;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.model.*;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Group;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Line;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Node;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.TPRelation;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class DocumentFlowService extends DocumentFlowServiceBase {

    private static final Logger logger = LoggerFactory.getLogger(DocumentFlowService.class);

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    public DocumentFlow generateDocumentFlow(UUID salesOrderId) {
        SalesOrder salesOrder = querySalesOrder(salesOrderId);
        Pair<List<Object>, List<TPRelation>> bfsResult = broadFirstSearch(salesOrder);
        List<Object> tps = bfsResult.getLeft();
        List<TPRelation> tpRelations = bfsResult.getRight();

        List<Group> groups = initGroups();
        List<Node> nodes = generateNodes(tps);
        List<Line> lines = generateLines(nodes, tpRelations);
        updateGroups(groups, nodes);

        DocumentFlow flow = new DocumentFlow();
        flow.setNodes(nodes);
        flow.setLines(lines);
        flow.setGroups(groups);

        return flow;
    }

    private SalesOrder querySalesOrder(UUID salesOrderId) {
        String query = String.format("/SalesOrder(guid'%s')?&$expand=salesOrderItemTPs/salesOrderItem/deliveryItemTPs/deliveryItem" +
                "/delivery/shipmentTPs/shipment/resourceTPs/resource", salesOrderId);
        return gttCoreServiceClient.readEntity(query, SalesOrder.class);
    }

    /***
     * Use broad first search algorithm to loop the structure of a sale order
     * @param salesOrder Object of a sales order
     * @return A list of objects (tps related to the sales order) and a list of tp relations (relationship between tps)
     */
    private Pair<List<Object>, List<TPRelation>> broadFirstSearch(SalesOrder salesOrder) {
        Set<UUID> visitedTPIds = new HashSet<>();
        Queue<Object> queue = new LinkedList<>();
        List<Object> tps = new LinkedList<>();
        List<TPRelation> tpRelations = new ArrayList<>();

        queue.add(salesOrder);
        while (!queue.isEmpty()) {
            Object tp = queue.poll();
            if (!isTPVisited(tp, visitedTPIds)) {
                visitedTPIds.add(getTPId(tp));
                tps.add(tp);

                Pair<List<Object>, List<TPRelation>> childTPs = findChildTPs(tp);
                queue.addAll(childTPs.getLeft());
                tpRelations.addAll(childTPs.getRight());
            }
        }
        return new ImmutablePair<>(tps, tpRelations);
    }

    private boolean isTPVisited(Object node, Set<UUID> visitedTPIds) {
        UUID id = getTPId(node);
        return visitedTPIds.contains(id);
    }

    private UUID getTPId(Object node) {
        try {
            Method method = node.getClass().getMethod("getId");
            return (UUID) method.invoke(node);
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            logger.error("Failed to invoke getId method.");
            throw new SOFServiceException(SOFServiceException.MESSAGE_CODE_GET_TP_ID_FAILED);
        }
    }

    /***
     * Generate the nodes that document flow requires from list of tps
     * @param tps All tps related to the sales order
     * @return List of nodes
     */
    private List<Node> generateNodes(List<Object> tps) {
        List<Node> nodes = new ArrayList<>();
        int index = 1;
        for (Object tp : tps) {
            Node node = generateNode(tp);
            node.setKey(index++);
            nodes.add(node);
        }
        return nodes;
    }

    private Node generateNode(Object tp) {
        Node node = new Node();
        if (tp instanceof SalesOrder) {
            node = generateSalesOrderNode((SalesOrder) tp);
        } else if (tp instanceof SalesOrderItem) {
            node = generateSalesOrderItemNode((SalesOrderItem) tp);
        } else if (tp instanceof DeliveryItem) {
            node = generateDeliveryItemNode((DeliveryItem) tp);
        } else if (tp instanceof Delivery) {
            node = generateDeliveryNode((Delivery) tp);
        } else if (tp instanceof Shipment) {
            node = generateShipmentNode((Shipment) tp);
        } else if (tp instanceof Resource) {
            node = generateResourceNode((Resource) tp);
        }
        return node;
    }


    /***
     * Generate the lines with nodes and relationship between nodes(tps)
     * @param nodes Generated nodes of document flow
     * @param tpRelations Relationship between nodes(tps)
     * @return List of lines
     */
    private List<Line> generateLines(List<Node> nodes, List<TPRelation> tpRelations) {
        List<Line> lines = new ArrayList<>();
        Map<UUID, Integer> nodeKeyMap = nodes.stream().collect(Collectors.toMap(Node::getId, Node::getKey));
        Map<UUID, Integer> nodeGroupMap = nodes.stream().collect(Collectors.toMap(Node::getId, Node::getGroup));
        tpRelations.forEach(tpRelation -> lines.add(generateLine(tpRelation, nodeKeyMap, nodeGroupMap)));
        return lines;
    }

    private Line generateLine(TPRelation tpRelation, Map<UUID, Integer> nodeKeyMap, Map<UUID, Integer> nodeGroupMap) {
        Line line = new Line();
        UUID fromNodeUUID = tpRelation.getFromTrackedProcessUUID();
        UUID toNodeUUID = tpRelation.getToTrackedProcessUUID();
        line.setFrom(nodeKeyMap.get(fromNodeUUID));
        line.setTo(nodeKeyMap.get(toNodeUUID));
        // Line status between sales order and sales order item is determined by isDelayed (of sale order item),
        // others are determined by process status of "TO" node
        if (nodeGroupMap.get(toNodeUUID).compareTo(DocumentFlowGroupEnum.SALES_ORDER_ITEM.getGroupKey()) <= 0) {
            line.setStatus(getGeneralStatusByIsDelayed(Boolean.valueOf(tpRelation.getToTrackedProcessStatus())).getStatus());
        } else {
            line.setStatus(getGeneralStatusByProcessStatus(tpRelation.getToTrackedProcessStatus()).getStatus());
        }
        return line;
    }

    private Pair<List<Object>, List<TPRelation>> findChildTPs(Object tp) {
        List<Object> tps = new ArrayList<>();
        List<TPRelation> tpRelations = new ArrayList<>();

        if (tp instanceof SalesOrder) {
            List<SalesOrderItemTP> salesOrderItemTPs = ((SalesOrder) tp).getSalesOrderItemTPs();
            List<SalesOrderItem> salesOrderItems = salesOrderItemTPs.stream()
                    .map(SalesOrderItemTP::getSalesOrderItem)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(salesOrderItems);
            salesOrderItems.forEach(item -> tpRelations.add(new TPRelation(((SalesOrder) tp).getId(), item.getId(),
                    Optional.ofNullable(item.getDelayed()).map(str -> String.valueOf(item.getDelayed())).orElse(String.valueOf(Boolean.FALSE)))));

        } else if (tp instanceof SalesOrderItem) {
            List<SalesOrderItemDeliveryItemTP> deliveryItemTPs = ((SalesOrderItem) tp).getDeliveryItemTPs();
            List<DeliveryItem> deliveryItems = deliveryItemTPs.stream()
                    .map(SalesOrderItemDeliveryItemTP::getDeliveryItem)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(deliveryItems);
            deliveryItems.forEach(item -> tpRelations.add(new TPRelation(((SalesOrderItem) tp).getId(), item.getId(), item.getProcessStatusCode())));

        } else if (tp instanceof DeliveryItem) {
            Optional<Delivery> opt = Optional.ofNullable(((DeliveryItem) tp).getDelivery());
            if (opt.isPresent()) {
                Delivery delivery = opt.get();
                tps.add(delivery);
                tpRelations.add(new TPRelation(((DeliveryItem) tp).getId(), delivery.getId(), delivery.getProcessStatusCode()));
            }

        } else if (tp instanceof Delivery) {
            List<ShipmentTP> shipmentTPs = ((Delivery) tp).getShipmentTPs();
            List<Shipment> shipments = shipmentTPs.stream()
                    .map(ShipmentTP::getShipment).filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(shipments);
            shipments.forEach(item -> tpRelations.add(new TPRelation(((Delivery) tp).getId(), item.getId(), item.getProcessStatusCode())));

        } else if (tp instanceof Shipment) {
            List<ResourceTP> resourceTPs = ((Shipment) tp).getResourceTPs();
            List<Resource> resources = resourceTPs.stream()
                    .map(ResourceTP::getResource).filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(resources);
            resources.forEach(item -> tpRelations.add(new TPRelation(((Shipment) tp).getId(), item.getId(), item.getProcessStatusCode())));
        }

        return new ImmutablePair<>(tps, tpRelations);
    }

    /***
     * Update the group status by looping the nodes
     * @param groups Generated groups of document flow
     * @param nodes Generated nodes of document flow
     */
    private void updateGroups(List<Group> groups, List<Node> nodes) {
        Map<Integer, List<Node>> map = nodes.stream().collect(Collectors.groupingBy(Node::getGroup));
        groups.forEach(group -> updateGroupStatus(group, map.get(group.getKey())));
    }

    private void updateGroupStatus(Group group, List<Node> groupNodes) {
        Set<String> allNodesStatus = new HashSet<>();
        String groupStatusError = DocumentFlowGeneralStatusEnum.ERROR.getStatus();

        // When groupNodes is null, which means no nodes under group, not necessary to update group status
        if (groupNodes == null) {
            return;
        }
        groupNodes.forEach(node -> allNodesStatus.add(node.getStatus()));
        if (allNodesStatus.contains(groupStatusError)) {
            group.setStatus(groupStatusError);
        }
    }

}