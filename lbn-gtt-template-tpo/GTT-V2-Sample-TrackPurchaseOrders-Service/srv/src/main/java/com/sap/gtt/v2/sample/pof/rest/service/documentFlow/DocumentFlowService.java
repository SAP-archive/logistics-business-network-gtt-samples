package com.sap.gtt.v2.sample.pof.rest.service.documentFlow;


import com.sap.gtt.v2.sample.pof.constant.DocumentFlowGeneralStatusEnum;
import com.sap.gtt.v2.sample.pof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDelivery;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemTP;
import com.sap.gtt.v2.sample.pof.odata.model.Resource;
import com.sap.gtt.v2.sample.pof.odata.model.ResourceTP;
import com.sap.gtt.v2.sample.pof.odata.model.Shipment;
import com.sap.gtt.v2.sample.pof.odata.model.ShipmentTP;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.Group;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.Line;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.Node;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.TPRelation;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.DocumentFlowGeneralStatusEnum.INFORMATION;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.INBOUND_DELIVERY;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.INBOUND_DELIVERY_ITEM;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.PURCHASE_ORDER;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.PURCHASE_ORDER_ITEM;
import static com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum.SHIPMENT;

@Service
public class DocumentFlowService {

    private static final String EXPAND_VALUE = "purchaseOrderItemTPs/purchaseOrderItem/inboundDeliveryItems/inboundDeliveryItem/inboundDelivery" +
            "/shipmentTPs/shipment/resourceTPs/resource";
    private static final String PURCHASE_ORDER_URL_TEMPLATE = "/PurchaseOrder(guid'%s')";

    private final GTTCoreServiceClient gttCoreServiceClient;
    private final DocumentFlowConverter converter;

    public DocumentFlowService(GTTCoreServiceClient gttCoreServiceClient, DocumentFlowConverter converter) {
        this.gttCoreServiceClient = gttCoreServiceClient;
        this.converter = converter;
    }

    public DocumentFlow generateDocumentItemFlow(UUID purchaseOrderId, UUID purchaseOrderItemId) {
        PurchaseOrder purchaseOrder = queryPurchaseOrder(purchaseOrderId, purchaseOrderItemId);
        return generate(purchaseOrder);
    }

    public DocumentFlow generateDocumentFlow(UUID purchaseOrderId) {
        PurchaseOrder purchaseOrder = queryPurchaseOrder(purchaseOrderId);
        return generate(purchaseOrder);
    }

    private PurchaseOrder queryPurchaseOrder(UUID purchaseOrderId, UUID purchaseOrderItemId) {
        PurchaseOrder purchaseOrder = queryPurchaseOrder(purchaseOrderId);
        List<PurchaseOrderItemTP> items = purchaseOrder.getPurchaseOrderItemTPs().stream()
                .filter(poi -> poi.getPurchaseOrderItemId().equals(purchaseOrderItemId))
                .collect(Collectors.toList());
        purchaseOrder.setPurchaseOrderItemTPs(items);
        return purchaseOrder;
    }

    private PurchaseOrder queryPurchaseOrder(UUID purchaseOrderId) {
        String query = String.format(PURCHASE_ORDER_URL_TEMPLATE, purchaseOrderId);
        String uriString = UriComponentsBuilder.fromUriString(query).queryParam(EXPAND, EXPAND_VALUE).encode().toUriString();
        return gttCoreServiceClient.readEntity(uriString, PurchaseOrder.class);
    }

    private DocumentFlow generate(PurchaseOrder purchaseOrder) {
        Pair<List<TpDefinition>, List<TPRelation>> bfsResult = breadthFirstSearch(purchaseOrder);

        List<Node> nodes = converter.convertNodes(bfsResult.getLeft());
        List<TPRelation> tpRelations = bfsResult.getRight();
        updateNodesStatuses(tpRelations, nodes);

        DocumentFlow flow = new DocumentFlow();

        flow.setNodes(nodes);
        flow.setLines(generateLines(nodes, tpRelations));
        flow.setGroups(generateGroups(nodes));

        return flow;
    }

    private void updateNodesStatuses(List<TPRelation> tpRelations, List<Node> nodes) {
        Map<UUID, Node> idNodeMap = nodes.stream().collect(Collectors.toMap(Node::getId, Function.identity()));
        Map<UUID, List<TPRelation>> toTrackedProcessRelationMap = tpRelations.stream().collect(Collectors.groupingBy(TPRelation::getToTrackedProcessUUID));

        List<Node> orderedNodes = nodes.stream().sorted((left, right) -> Integer.compare(right.getGroup(), left.getGroup())).collect(Collectors.toList());

        for (Node toNode : orderedNodes) {
            List<TPRelation> tpRelationsByToTrackedProcess = toTrackedProcessRelationMap.getOrDefault(toNode.getId(), Collections.emptyList());
            for (TPRelation tpRelation : tpRelationsByToTrackedProcess) {
                Node fromNode = idNodeMap.get(tpRelation.getFromTrackedProcessUUID());
                if (DocumentFlowGeneralStatusEnum.compareByStatuses(fromNode.getStatus(), toNode.getStatus()) < 0) {
                    fromNode.setStatus(toNode.getStatus());
                }
            }
        }
    }

    /***
     * Use breadth first search algorithm to loop the structure of a purchase order
     * @param purchaseOrder Object of a purchase order
     * @return A list of objects (tps related to the purchase order) and a list of tp relations (relationship between tps)
     */
    private Pair<List<TpDefinition>, List<TPRelation>> breadthFirstSearch(PurchaseOrder purchaseOrder) {
        Set<UUID> visitedTPIds = new HashSet<>();
        Queue<TpDefinition> queue = new LinkedList<>();
        List<TpDefinition> nodes = new LinkedList<>();
        List<TPRelation> tpRelations = new LinkedList<>();

        queue.add(new TpDefinition(purchaseOrder));
        while (!queue.isEmpty()) {
            TpDefinition tp = queue.poll();
            if (!visitedTPIds.contains(tp.getId())) {
                visitedTPIds.add(tp.getId());
                nodes.add(tp);

                Pair<List<TpDefinition>, List<TPRelation>> childTPs = findChildTPs(tp);
                queue.addAll(childTPs.getLeft());
                tpRelations.addAll(childTPs.getRight());
            }
        }
        return Pair.of(nodes, tpRelations);
    }

    private Pair<List<TpDefinition>, List<TPRelation>> findChildTPs(TpDefinition tpDefinition) {
        List<TpDefinition> tps = new ArrayList<>();
        List<TPRelation> tpRelations = new ArrayList<>();

        if (PURCHASE_ORDER.getTrackingIdType().equals(tpDefinition.getTrackingIdType())) {
            PurchaseOrder purchaseOrder = (PurchaseOrder) tpDefinition.getTp();
            List<PurchaseOrderItemTP> purchaseOrderTPs = purchaseOrder.getPurchaseOrderItemTPs();
            List<PurchaseOrderItem> purchaseOrderItems = purchaseOrderTPs.stream()
                    .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(purchaseOrderItems.stream().map(TpDefinition::new).collect(Collectors.toList()));
            purchaseOrderItems.forEach(item -> tpRelations.add(new TPRelation(purchaseOrder.getId(), item.getId(), purchaseOrder.getProcessStatusCode())));
        } else if (PURCHASE_ORDER_ITEM.getTrackingIdType().equals(tpDefinition.getTrackingIdType())) {
            PurchaseOrderItem purchaseOrderItem = (PurchaseOrderItem) tpDefinition.getTp();
            List<InboundDeliveryItem> inboundDeliveryItems = purchaseOrderItem.getInboundDeliveryItems().stream()
                    .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(inboundDeliveryItems.stream().map(TpDefinition::new).collect(Collectors.toList()));
            inboundDeliveryItems
                    .forEach(item -> tpRelations.add(new TPRelation(purchaseOrderItem.getId(), item.getId(), purchaseOrderItem.getProcessStatusCode())));
        } else if (INBOUND_DELIVERY_ITEM.getTrackingIdType().equals(tpDefinition.getTrackingIdType())) {
            InboundDeliveryItem inboundDeliveryItem = (InboundDeliveryItem) tpDefinition.getTp();
            InboundDelivery delivery = inboundDeliveryItem.getInboundDelivery();
            tps.add(new TpDefinition(delivery));
            tpRelations.add(new TPRelation(inboundDeliveryItem.getId(), delivery.getId(), inboundDeliveryItem.getProcessStatusCode()));
        } else if (INBOUND_DELIVERY.getTrackingIdType().equals(tpDefinition.getTrackingIdType())) {
            InboundDelivery inboundDelivery = (InboundDelivery) tpDefinition.getTp();
            List<Shipment> shipments = inboundDelivery.getShipmentTPs().stream()
                    .map(ShipmentTP::getShipment)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            tps.addAll(shipments.stream().map(TpDefinition::new).collect(Collectors.toList()));
            shipments.forEach(item -> tpRelations.add(new TPRelation(inboundDelivery.getId(), item.getId(), inboundDelivery.getProcessStatusCode())));
        } else if (SHIPMENT.getTrackingIdType().equals(tpDefinition.getTrackingIdType())) {
            Shipment shipment = (Shipment) tpDefinition.getTp();
            List<Resource> resources = shipment.getResourceTPs().stream()
                    .map(ResourceTP::getResource)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
            tps.addAll(resources.stream().map(TpDefinition::new).collect(Collectors.toList()));
            resources.forEach(item -> tpRelations.add(new TPRelation(shipment.getId(), item.getId(), shipment.getProcessStatusCode())));
        }

        return new ImmutablePair<>(tps, tpRelations);
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
        tpRelations.forEach(tpRelation -> lines.add(generateLine(tpRelation, nodeKeyMap)));
        return lines;
    }

    private Line generateLine(TPRelation tpRelation, Map<UUID, Integer> nodeKeyMap) {
        Line line = new Line();
        UUID fromNodeUUID = tpRelation.getFromTrackedProcessUUID();
        UUID toNodeUUID = tpRelation.getToTrackedProcessUUID();
        line.setFrom(nodeKeyMap.get(fromNodeUUID));
        line.setTo(nodeKeyMap.get(toNodeUUID));
        line.setStatus(converter.convertProcessStatus(tpRelation.getToTrackedProcessStatus()).getStatus());
        return line;
    }

    /***
     * Initialize the groups of document flow
     * @return List of groups
     */
    private List<Group> generateGroups(List<Node> nodes) {
        Map<Integer, List<Node>> map = nodes.stream().collect(Collectors.groupingBy(Node::getGroup));
        return Arrays.stream(DocumentFlowGroupEnum.values()).map(groupDef -> {
            Group group = new Group();
            group.setTitle(groupDef.getGroupTitle());
            group.setKey(groupDef.getGroupKey());
            group.setStatus(getGeneralGroupStatus(map, groupDef.getGroupKey()));
            return group;
        }).collect(Collectors.toList());
    }

    private String getGeneralGroupStatus(Map<Integer, List<Node>> map, Integer groupKey) {
        List<Node> nodes = map.getOrDefault(groupKey, Collections.emptyList());
        Optional<String> closestToErrorStatus = nodes.stream().map(Node::getStatus).max(DocumentFlowGeneralStatusEnum::compareByStatuses);
        return closestToErrorStatus.orElseGet(INFORMATION::getStatus);
    }
}