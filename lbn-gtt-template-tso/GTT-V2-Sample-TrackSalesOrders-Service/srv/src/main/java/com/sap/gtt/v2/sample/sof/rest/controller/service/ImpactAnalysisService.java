package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.domain.DelayEvent;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.domain.TrackedProcess;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.*;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.DocumentFlow;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Line;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Node;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static com.sap.gtt.v2.sample.sof.constant.Constants.ID;
import static com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient.*;

@Service
public class ImpactAnalysisService extends DocumentFlowServiceBase {

    public static final String OUTBOUND_DELIVERY = "OUTBOUND_DELIVERY";
    public static final String OUTBOUND_DELIVERY_IT = "OUTBOUND_DELIVERY_IT";
    public static final String SALES_ORDER_ITEM = "SALES_ORDER_ITEM";
    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    public DocumentFlow getInitialNodes(UUID deliveryItemId, UUID plannedEventId) {
        String query = UriComponentsBuilder.fromUriString("/Delay")
                .queryParam(FILTER, String.format("eventProcesses/process_id eq guid'%s' and eventProcesses/plannedEvent_id eq guid'%s'", deliveryItemId, plannedEventId))
                .queryParam(EXPAND, "eventProcesses/process")
                .queryParam(TOP, 1)
                .queryParam(ORDERBY, "actualBusinessTimestamp desc")
                .build().encode().toUriString();

        ODataResultList<DelayEvent> res = gttCoreServiceClient.readEntitySet(query, DelayEvent.class);
        if (res.getResults().size() != 1) {
            throw new SOFServiceException(SOFServiceException.MESSAGE_CODE_ERROR_NO_DELAY_EVENT_FOUND);
        }
        DelayEvent delayEvent = res.getResults().get(0);

        Shipment shipment = null;
        List<UUID> deliveryIds = new ArrayList<>();
        for (ProcessEventDirectory processEventDirectory : delayEvent.getEventProcesses()) {
            TrackedProcess trackedProcess = processEventDirectory.getProcess();
            if (trackedProcess.getTrackingIdType().equals("SHIPMENT_ORDER")) {
                query = UriComponentsBuilder.fromUriString("/Shipment(guid'" + trackedProcess.getId() + "')")
                        .build().encode().toUriString();
                shipment = gttCoreServiceClient.readEntity(query, Shipment.class);
            } else if (trackedProcess.getTrackingIdType().equals(OUTBOUND_DELIVERY)) {
                deliveryIds.add(trackedProcess.getId());
            }
        }

        List<FilterCondition> conditions = new ArrayList<>();
        deliveryIds.forEach(deliveryId ->
                conditions.add(new FilterCondition(ID, FilterCondition.EDM_TYPE_GUID, deliveryId.toString(), BinaryOperator.EQ)));
        FilterExpression filter = FilterExpressionBuilder.createFilterExpression(conditions, BinaryOperator.OR);

        List<Delivery> deliveries = new ArrayList<>();
        if (filter != null) {
            query = UriComponentsBuilder.fromUriString("/Delivery")
                    .queryParam(FILTER, filter.getExpressionString())
                    .build().encode().toUriString();
            ODataResultList<Delivery> deliveryResults = gttCoreServiceClient.readEntitySetAll(query, Delivery.class);
            deliveries = deliveryResults.getResults();
        }

        if (shipment != null) {
            return generateDocumentFlowStartsFromShipment(shipment, deliveries);
        } else {
            if (!deliveries.isEmpty()) {
                return generateDocumentFlowStartsFromDelivery(deliveries.get(0));
            } else {
                return generateDocumentFlowStartsFromDeliveryItem(deliveryItemId);
            }
        }
    }

    private DocumentFlow generateDocumentFlowStartsFromShipment(Shipment shipment, List<Delivery> deliveries) {
        int key = 0;
        List<Node> nodes = new ArrayList<>();
        Node shipmentNode = generateShipmentNode(shipment);
        shipmentNode.setKey(key++);
        nodes.add(shipmentNode);
        List<Line> lines = new ArrayList<>();
        for (Delivery delivery : deliveries) {
            Node deliveryNode = generateDeliveryNode(delivery);
            deliveryNode.setKey(key++);
            nodes.add(deliveryNode);

            fillLine(lines, shipmentNode, deliveryNode);
        }

        DocumentFlow documentFlow = generateDocumentFlow(nodes, lines);
        return documentFlow;
    }

    private DocumentFlow generateDocumentFlowStartsFromDelivery(Delivery delivery) {
        List<Node> nodes = new ArrayList<>();
        List<Line> lines = new ArrayList<>();
        int key = 0;
        Node deliveryNode = generateDeliveryNode(delivery);
        deliveryNode.setKey(key++);
        nodes.add(deliveryNode);
        List<Node> deliveryItemNodes = generateNextNodesForDelivery(deliveryNode.getId());
        nodes.addAll(deliveryItemNodes);
        for (Node deliveryItemNode : deliveryItemNodes) {
            deliveryItemNode.setKey(key++);
            fillLine(lines, deliveryNode, deliveryItemNode);
        }

        DocumentFlow documentFlow = generateDocumentFlow(nodes, lines);

        return documentFlow;
    }

    private DocumentFlow generateDocumentFlowStartsFromDeliveryItem(UUID deliveryItemId) {
        String query = UriComponentsBuilder.fromUriString("/DeliveryItem(guid'" + deliveryItemId + "')")
                .build().encode().toUriString();
        DeliveryItem deliveryItem = gttCoreServiceClient.readEntity(query, DeliveryItem.class);
        List<Node> nodes = new ArrayList<>();
        List<Line> lines = new ArrayList<>();
        int key = 0;
        Node deliveryItemNode = generateDeliveryItemNode(deliveryItem);
        deliveryItemNode.setKey(key++);
        nodes.add(deliveryItemNode);
        addNodesAndLinesStartsFromDeliveryItem(nodes, lines, key, deliveryItemNode);

        DocumentFlow documentFlow = generateDocumentFlow(nodes, lines);
        return documentFlow;
    }

    private DocumentFlow generateDocumentFlow(List<Node> nodes, List<Line> lines) {
        DocumentFlow documentFlow = new DocumentFlow();
        documentFlow.setGroups(initGroups());
        documentFlow.setNodes(nodes);
        documentFlow.setLines(lines);
        return documentFlow;
    }

    private int addNodesAndLinesStartsFromDeliveryItem(List<Node> nodes, List<Line> lines, int key, Node deliveryItemNode) {
        List<Node> salesOrderItemNodes = generateNextNodesForDeliveryItem(deliveryItemNode.getId());
        nodes.addAll(salesOrderItemNodes);
        for (Node salesOrderItemNode : salesOrderItemNodes) {
            salesOrderItemNode.setKey(key++);
            fillLine(lines, deliveryItemNode, salesOrderItemNode);
        }
        return key;
    }

    private void fillLine(List<Line> lines, Node fromNode, Node toNode) {
        Line line;
        line = new Line();
        line.setFrom(fromNode.getKey());
        line.setTo(toNode.getKey());
        line.setStatus(toNode.getStatus());
        lines.add(line);
    }

    public List<Node> getNextNodes(UUID tpId, String trackingIdType) {
        switch (trackingIdType) {
            case OUTBOUND_DELIVERY:
                return generateNextNodesForDelivery(tpId);
            case OUTBOUND_DELIVERY_IT:
                return generateNextNodesForDeliveryItem(tpId);
            case SALES_ORDER_ITEM:
                return generateNextNodesForSalesOrderItem(tpId);
            default:
                throw new SOFServiceException(SOFServiceException.MESSAGE_CODE_UNSUPPORTED_TRACKING_ID_TYPE, new Object[]{trackingIdType});
        }
    }

    private List<Node> generateNextNodesForDelivery(UUID tpId) {
        List<Node> res = new ArrayList<>();
        String query = UriComponentsBuilder.fromUriString("/Delivery(guid'" + tpId + "')")
                .queryParam(EXPAND, "deliveryItems")
                .build().encode().toUriString();

        Delivery delivery = gttCoreServiceClient.readEntity(query, Delivery.class);
        for (DeliveryItem deliveryItem: delivery.getDeliveryItems()) {
            if (deliveryItem == null) continue;
            Node node = generateDeliveryItemNode(deliveryItem);
            res.add(node);
        }

        return res;
    }

    private List<Node> generateNextNodesForDeliveryItem(UUID tpId) {
        List<Node> res = new ArrayList<>();
        String query = UriComponentsBuilder.fromUriString("/DeliveryItem(guid'" + tpId + "')")
                .queryParam(EXPAND, "salesOrderItem")
                .build().encode().toUriString();

        DeliveryItem deliveryItem = gttCoreServiceClient.readEntity(query, DeliveryItem.class);
        Node node = generateSalesOrderItemNode(deliveryItem.getSalesOrderItem());
        res.add(node);

        return res;
    }

    private List<Node> generateNextNodesForSalesOrderItem(UUID tpId) {
        List<Node> res = new ArrayList<>();
        String query = UriComponentsBuilder.fromUriString("/SalesOrderItem(guid'" + tpId + "')")
                .queryParam(EXPAND, "salesOrder")
                .build().encode().toUriString();

        SalesOrderItem salesOrderItem = gttCoreServiceClient.readEntity(query, SalesOrderItem.class);
        Node node = generateSalesOrderNode(salesOrderItem.getSalesOrder());
        res.add(node);

        return res;
    }
}
