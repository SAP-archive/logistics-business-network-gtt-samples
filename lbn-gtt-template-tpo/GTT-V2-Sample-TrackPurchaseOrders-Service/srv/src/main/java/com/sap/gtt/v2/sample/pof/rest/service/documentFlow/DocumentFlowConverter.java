package com.sap.gtt.v2.sample.pof.rest.service.documentFlow;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.constant.DocumentFlowAttributeValueStatusEnum;
import com.sap.gtt.v2.sample.pof.constant.DocumentFlowGeneralStatusEnum;
import com.sap.gtt.v2.sample.pof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.pof.constant.TrackingIdTypeEnum;
import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.handler.POFLocationODataHandler;
import com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderItemODataHandler;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDelivery;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.Resource;
import com.sap.gtt.v2.sample.pof.odata.model.Shipment;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.Attribute;
import com.sap.gtt.v2.sample.pof.rest.domain.documentflow.Node;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isBlank;

@Component
public class DocumentFlowConverter {

    private static final String REGEX_LEADING_ZERO = "^0*";
    private static final String SLASH = "/";
    private static final String RECEIVING_LOCATION_ID = "receivingLocationId";
    private static final String NET_VALUE = "netValue";
    private static final String MATERIAL_ID = "materialId";
    private static final String MATERIAL_NUMBER = "materialNumber";
    private static final String MATERIAL_DESCRIPTION = "materialDescription";
    private static final String ORDER_QUANTITY = "orderQuantity";
    private static final String ITEM_DESCRIPTION = "itemDescription";
    private static final String PLANNED_DELIVERY_DATE = "plannedDeliveryDate";
    private static final String PROCESS_STATUS = "processStatus_code";
    private static final String EXECUTION_STATUS = "executionStatus_code";

    @Autowired
    private POFLocationODataHandler locationODataHandler;

    @Autowired
    private POFPurchaseOrderItemODataHandler purchaseOrderItemODataHandler;

    public List<Node> convertNodes(List<TpDefinition> tpDefinitions) {
        AtomicInteger index = new AtomicInteger();
        return tpDefinitions.stream()
                .map(tpDefinition -> convertNode(tpDefinition, index.incrementAndGet()))
                .collect(Collectors.toList());
    }

    public Node convertNode(TpDefinition tpDefinition, Integer index) {
        Node node = new Node();
        node.setKey(index);
        node.setTrackingIdType(tpDefinition.getTrackingIdType());
        node.setId(tpDefinition.getId());

        switch (TrackingIdTypeEnum.getTrackingIdTypeEnum(tpDefinition.getTrackingIdType())) {
            case PURCHASE_ORDER:
                return generatePurchaseOrderNode(node, (PurchaseOrder) tpDefinition.getTp());
            case INBOUND_DELIVERY_ITEM:
                return generateInboundDeliveryItemNode(node, (InboundDeliveryItem) tpDefinition.getTp());
            case INBOUND_DELIVERY:
                return generateInboundDeliverNode(node, (InboundDelivery) tpDefinition.getTp());
            case PURCHASE_ORDER_ITEM:
                return generatePurchaseOrderItemNode(node, (PurchaseOrderItem) tpDefinition.getTp());
            case SHIPMENT:
                return generateShipmentNode(node, (Shipment) tpDefinition.getTp());
            case RESOURCE:
                return generateResourceNode(node, (Resource) tpDefinition.getTp());
            default:
                throw new POFServiceException(format("Unsupported Node type: %s", tpDefinition.getTrackingIdType()));
        }
    }

    public DocumentFlowGeneralStatusEnum convertProcessStatus(String status) {
        switch (status) {
            case Constants.PROCESS_STATUS_OVERDUE:
            case Constants.PROCESS_STATUS_LATE:
            case Constants.PROCESS_STATUS_EARLY:
                return DocumentFlowGeneralStatusEnum.WARNING;
            case Constants.PROCESS_STATUS_DELAYED:
                return DocumentFlowGeneralStatusEnum.ERROR;
            case Constants.PROCESS_STATUS_AS_PLANNED:
                return DocumentFlowGeneralStatusEnum.SUCCESS;
            default:
                return DocumentFlowGeneralStatusEnum.INFORMATION;
        }
    }

    private Node generateResourceNode(Node node, Resource tp) {
        Integer groupKey = DocumentFlowGroupEnum.RESOURCE.getGroupKey();

        String title = getStringOrDefault(tp.getTrackingId(), EMPTY);
        node.setTitle(title.replaceAll(REGEX_LEADING_ZERO, EMPTY));
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());

        List<Attribute> attributes = Collections.singletonList(
                new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, convertAttributeStatus(tp.getProcessStatusCode()).getStatus(), groupKey)
        );
        node.setAttributes(attributes);

        return node;
    }

    private Node generateShipmentNode(Node node, Shipment tp) {
        Integer groupKey = DocumentFlowGroupEnum.SHIPMENT.getGroupKey();

        String title = getStringOrDefault(tp.getTrackingId(), EMPTY);
        node.setTitle(title.replaceAll(REGEX_LEADING_ZERO, EMPTY));
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());

        List<Attribute> attributes = Arrays.asList(
                new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, convertAttributeStatus(tp.getProcessStatusCode()).getStatus(), groupKey),
                new Attribute(EXECUTION_STATUS, tp.getExecutionStatusCode(), null, null, groupKey)
        );
        node.setAttributes(attributes);

        return node;
    }

    private Node generateInboundDeliveryItemNode(Node node, InboundDeliveryItem tp) {
        Integer groupKey = DocumentFlowGroupEnum.INBOUND_DELIVERY_ITEM.getGroupKey();

        String inboundDeliveryItemNo = getStringOrDefault(tp.getItemNo(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String inboundDeliveryNo = getStringOrDefault(tp.getInboundDeliveryNo(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String title = format("%s %s %s", inboundDeliveryNo, SLASH, inboundDeliveryItemNo);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());

        String materialNumber = getStringOrDefault(tp.getMaterialNumber(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String orderQuantity = getStringOrDefault(tp.getOrderQuantity(), EMPTY);
        List<Attribute> attributes = Arrays.asList(
                new Attribute(MATERIAL_NUMBER, materialNumber, null, null, groupKey),
                new Attribute(ITEM_DESCRIPTION, tp.getItemDescription(), null, null, groupKey),
                new Attribute(ORDER_QUANTITY, orderQuantity, tp.getOrderQuantityUoM(), null, groupKey),
                new Attribute(PLANNED_DELIVERY_DATE, getStringOrDefault(tp.getPlannedDeliveryDate(), EMPTY), null, null, groupKey),
                new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, convertAttributeStatus(tp.getProcessStatusCode()).getStatus(), groupKey),
                new Attribute(EXECUTION_STATUS, tp.getExecutionStatusCode(), null, null, groupKey)
        );
        node.setAttributes(attributes);

        return node;
    }

    private Node generateInboundDeliverNode(Node node, InboundDelivery tp) {
        Integer groupKey = DocumentFlowGroupEnum.INBOUND_DELIVERY.getGroupKey();

        String title = tp.getInboundDeliveryNo().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());
        node.setAttributes(Collections.singletonList(
                new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, convertAttributeStatus(tp.getProcessStatusCode()).getStatus(), groupKey)
        ));

        return node;
    }

    private Node generatePurchaseOrderNode(Node node, PurchaseOrder tp) {
        Integer groupKey = DocumentFlowGroupEnum.PURCHASE_ORDER.getGroupKey();

        String title = tp.getPurchaseOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());

        String netValue = getStringOrDefault(tp.getNetValue(), EMPTY);
        String location = getLocationInformation(tp.getPartyId(), tp.getLogicalSystem(), tp.getReceivingLocationTypeCode(), tp.getReceivingLocationId());
        List<Attribute> attributes = Arrays.asList(
                new Attribute(RECEIVING_LOCATION_ID, location, null, null, groupKey),
                new Attribute(NET_VALUE, netValue, tp.getCurrency(), null, groupKey)
        );
        node.setAttributes(attributes);

        return node;
    }

    private Node generatePurchaseOrderItemNode(Node node, PurchaseOrderItem tp) {
        Integer groupKey = DocumentFlowGroupEnum.PURCHASE_ORDER_ITEM.getGroupKey();

        String purchaseOrderItemNo = getStringOrDefault(tp.getItemNo(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String purchaseOrderNo = getStringOrDefault(tp.getPurchaseOrderNo(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String title = format("%s %s %s", purchaseOrderNo, SLASH, purchaseOrderItemNo);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(convertProcessStatus(tp.getProcessStatusCode()).getStatus());

        purchaseOrderItemODataHandler.updateCompletionValue(tp);

        String orderQuantity = getStringOrDefault(tp.getOrderQuantity(), EMPTY);
        String netValue = getStringOrDefault(tp.getNetValue(), EMPTY);
        String materialId = getStringOrDefault(tp.getMaterialId(), EMPTY).replaceAll(REGEX_LEADING_ZERO, EMPTY);
        String location = getLocationInformation(tp.getPartyId(), tp.getLogicalSystem(), tp.getReceivingLocationTypeCode(), tp.getReceivingLocationId());
        List<Attribute> attributes = Arrays.asList(
                new Attribute(MATERIAL_ID, materialId, null, null, groupKey),
                new Attribute(MATERIAL_DESCRIPTION, tp.getMaterialDescription(), null, null, groupKey),
                new Attribute(ORDER_QUANTITY, orderQuantity, tp.getOrderQuantityUoM(), null, groupKey),
                new Attribute(NET_VALUE, netValue, tp.getCurrency(), null, groupKey),
                new Attribute(RECEIVING_LOCATION_ID, location, null, null, groupKey)
        );
        node.setAttributes(attributes);

        return node;
    }

    private String getLocationInformation(String partyId, String system, String locationTypeCode, String locationId) {
        if (isBlank(locationTypeCode) || isBlank(locationId)) {
            return isBlank(locationId) ? EMPTY : locationId;
        }
        String locationAltKey = POFUtils.generateLocationAltKey(partyId, system, locationTypeCode, locationId);
        Location location = locationODataHandler.getLocation(locationAltKey);
        return isNull(location) ? locationId : location.getLocationDescription();
    }

    private String getStringOrDefault(Object o, String def) {
        return Optional.ofNullable(o).map(Object::toString).orElse(def);
    }

    private DocumentFlowAttributeValueStatusEnum convertAttributeStatus(String status) {
        switch (status) {
            case Constants.PROCESS_STATUS_OVERDUE:
            case Constants.PROCESS_STATUS_LATE:
            case Constants.PROCESS_STATUS_EARLY:
                return DocumentFlowAttributeValueStatusEnum.WARNING;
            case Constants.PROCESS_STATUS_DELAYED:
                return DocumentFlowAttributeValueStatusEnum.ERROR;
            case Constants.PROCESS_STATUS_AS_PLANNED:
                return DocumentFlowAttributeValueStatusEnum.SUCCESS;
            default:
                return DocumentFlowAttributeValueStatusEnum.INFORMATION;
        }
    }
}
