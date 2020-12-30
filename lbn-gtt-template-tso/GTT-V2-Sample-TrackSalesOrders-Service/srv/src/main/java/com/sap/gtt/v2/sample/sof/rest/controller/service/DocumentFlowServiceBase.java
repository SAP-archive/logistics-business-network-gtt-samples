package com.sap.gtt.v2.sample.sof.rest.controller.service;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowAttributeValueStatusEnum;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGeneralStatusEnum;
import com.sap.gtt.v2.sample.sof.constant.DocumentFlowGroupEnum;
import com.sap.gtt.v2.sample.sof.odata.model.*;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Attribute;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Group;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.documentflow.Node;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class DocumentFlowServiceBase {

    private static final String REGEX_LEADING_ZERO = "^0*";
    private static final String EMPTY = "";
    private static final String SLASH = "/";
    private static final String SHIP_TO_PARTY_ID = "shipToPartyId";
    private static final String NET_VALUE = "netValue";
    private static final String MATERIAL_NO = "materialNo";
    private static final String MATERIAL_DESCRIPTION = "materialDescription";
    private static final String ORDER_QUANTITY = "orderQuantity";
    private static final String PROCESS_STATUS = "processStatus";
    private static final String EXECUTION_STATUS = "executionStatus";
    private static final String IS_DELAYED = "isDelayed";

    /***
     * Initialize the groups of document flow
     * @return List of groups
     */
    protected List<Group> initGroups() {
        List<String> groupsTitle = EnumSet.allOf(DocumentFlowGroupEnum.class).stream()
                .map(DocumentFlowGroupEnum::getGroupTitle)
                .collect(Collectors.toList());
        return IntStream.range(0, groupsTitle.size()).mapToObj(index -> {
            Group group = new Group();
            group.setTitle(groupsTitle.get(index));
            group.setKey(index + 1);
            group.setStatus(DocumentFlowGeneralStatusEnum.INFORMATION.getStatus());
            group.setIcon(Constants.SAP_ICON_LEAD);
            return group;
        }).collect(Collectors.toList());
    }

    protected Node generateResourceNode(Resource tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.RESOURCE.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getTrackingId().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByProcessStatus(tp.getProcessStatusCode()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        String valueStatus = getAttributeStatusByProcessStatus(tp.getProcessStatusCode()).getStatus();
        Attribute attribute = new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, valueStatus, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected Node generateShipmentNode(Shipment tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.SHIPMENT.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getShipmentNo().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByProcessStatus(tp.getProcessStatusCode()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        String valueStatus = getAttributeStatusByProcessStatus(tp.getProcessStatusCode()).getStatus();
        Attribute attribute = new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, valueStatus, groupKey);
        attributes.add(attribute);
        attribute = new Attribute(EXECUTION_STATUS, tp.getExecutionStatusCode(), null, null, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected Node generateDeliveryNode(Delivery tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.DELIVERY.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getDeliveryNo().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByProcessStatus(tp.getProcessStatusCode()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        String valueStatus = getAttributeStatusByProcessStatus(tp.getProcessStatusCode()).getStatus();
        Attribute attribute = new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, valueStatus, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected Node generateDeliveryItemNode(DeliveryItem tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.DELIVERY_ITEM.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getDeliveryNo().replaceAll(REGEX_LEADING_ZERO, EMPTY).concat(SLASH)
                .concat(tp.getItemNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByProcessStatus(tp.getProcessStatusCode()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        Attribute attribute = new Attribute(MATERIAL_NO, tp.getMaterialNo(), null, null, groupKey);
        attributes.add(attribute);
        attribute = new Attribute(MATERIAL_DESCRIPTION, tp.getMaterialDescription(), null, null, groupKey);
        attributes.add(attribute);
        String orderQuantity = tp.getOrderQuantity() == null ? null : tp.getOrderQuantity().toString();
        attribute = new Attribute(ORDER_QUANTITY, orderQuantity, tp.getQuantityUoM(), null, groupKey);
        attributes.add(attribute);
        String valueStatus = getAttributeStatusByProcessStatus(tp.getProcessStatusCode()).getStatus();
        attribute = new Attribute(PROCESS_STATUS, tp.getProcessStatusCode(), null, valueStatus, groupKey);
        attributes.add(attribute);
        attribute = new Attribute(EXECUTION_STATUS, tp.getExecutionStatusCode(), null, null, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected Node generateSalesOrderItemNode(SalesOrderItem tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.SALES_ORDER_ITEM.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getSalesOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY).concat(SLASH)
                .concat(tp.getItemNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByIsDelayed(tp.getDelayed()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        Attribute attribute = new Attribute(MATERIAL_NO, tp.getMaterialNo(), null, null, groupKey);
        attributes.add(attribute);
        attribute = new Attribute(MATERIAL_DESCRIPTION, tp.getMaterialDescription(), null, null, groupKey);
        attributes.add(attribute);
        String netValue = tp.getNetValue() == null ? null : tp.getNetValue().toString();
        attribute = new Attribute(NET_VALUE, netValue, tp.getCurrency(), null, groupKey);
        attributes.add(attribute);
        String orderQuantity = tp.getOrderQuantity() == null ? null : tp.getOrderQuantity().toString();
        attribute = new Attribute(ORDER_QUANTITY, orderQuantity, tp.getUom(), null, groupKey);
        attributes.add(attribute);
        String isDelayed = Boolean.TRUE.equals(tp.getDelayed()) ? String.valueOf(tp.getDelayed()) : String.valueOf(Boolean.FALSE);
        String valueStatus = getAttributeStatusByIsDelayed(tp.getDelayed()).getStatus();
        attribute = new Attribute(IS_DELAYED, isDelayed, null, valueStatus, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected Node generateSalesOrderNode(SalesOrder tp) {
        Node node = new Node();
        List<Attribute> attributes = new ArrayList<>();
        Integer groupKey = DocumentFlowGroupEnum.SALES_ORDER.getGroupKey();

        node.setId(tp.getId());
        String title = tp.getSalesOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY);
        node.setTitle(title);
        node.setGroup(groupKey);
        node.setStatus(getGeneralStatusByIsDelayed(tp.getDelayed()).getStatus());
        node.setTrackingIdType(tp.getTrackingIdType());

        Attribute attribute = new Attribute(SHIP_TO_PARTY_ID, tp.getShipToPartyId(), null, null, groupKey);
        attributes.add(attribute);
        String netValue = tp.getNetValue() == null ? null : tp.getNetValue().toString();
        attribute = new Attribute(NET_VALUE, netValue, tp.getCurrency(), null, groupKey);
        attributes.add(attribute);
        String isDelayed = Boolean.TRUE.equals(tp.getDelayed()) ? String.valueOf(tp.getDelayed()) : String.valueOf(Boolean.FALSE);
        String valueStatus = getAttributeStatusByIsDelayed(tp.getDelayed()).getStatus();
        attribute = new Attribute(IS_DELAYED, isDelayed, null, valueStatus, groupKey);
        attributes.add(attribute);

        node.setAttributes(attributes);
        return node;
    }

    protected DocumentFlowGeneralStatusEnum getGeneralStatusByProcessStatus(String status) {
        switch (status) {
            case Constants.PROCESS_STATUS_OVERDUE:
            case Constants.PROCESS_STATUS_DELAYED:
                return DocumentFlowGeneralStatusEnum.ERROR;
            default:
                return DocumentFlowGeneralStatusEnum.INFORMATION;
        }
    }

    protected DocumentFlowGeneralStatusEnum getGeneralStatusByIsDelayed(Boolean isDelayed) {
        return Boolean.TRUE.equals(isDelayed) ? DocumentFlowGeneralStatusEnum.ERROR : DocumentFlowGeneralStatusEnum.INFORMATION;
    }

    protected DocumentFlowAttributeValueStatusEnum getAttributeStatusByProcessStatus(String status) {
        switch (status) {
            case Constants.PROCESS_STATUS_OVERDUE:
            case Constants.PROCESS_STATUS_DELAYED:
                return DocumentFlowAttributeValueStatusEnum.VALUE_STATUS_ERROR;
            default:
                return DocumentFlowAttributeValueStatusEnum.VALUE_STATUS_INFORMATION;
        }
    }

    protected DocumentFlowAttributeValueStatusEnum getAttributeStatusByIsDelayed(Boolean isDelayed) {
        return Boolean.TRUE.equals(isDelayed) ? DocumentFlowAttributeValueStatusEnum.VALUE_STATUS_ERROR :
                DocumentFlowAttributeValueStatusEnum.VALUE_STATUS_INFORMATION;
    }
}
