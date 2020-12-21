sap.ui.define([
], function () {
  "use strict";

  return Object.freeze({

    ENTITY_TYPES: {
      PURCHASE_ORDER        : "PurchaseOrder",
      PURCHASE_ORDER_ITEM   : "PurchaseOrderItem",
      INDOUND_DELIVERY_ITEM : "InboundDeliveryItem",
      INDOUND_DELIVERY      : "InboundDelivery",
      SHIPMENT              : "Shipment",
      RESOURCE              : "Resource",
    },

    TRACKING_ID_TYPE: {
      PURCHASE_ORDER        : "PURCHASE_ORDER",
      PURCHASE_ORDER_ITEM   : "PURCHASE_ORDER_ITEM",
      INDOUND_DELIVERY_ITEM : "INBOUND_DELIVERY_IT",
      INDOUND_DELIVERY      : "INBOUND_DELIVERY",
      INBOUND_SHIPMENT      : "INBOUND_SHIPMENT",
      RESOURCE              : "RESOURCE",
    },

    PO_ITEM_MATERIAL_ID_PROP  : "materialId",
    PO_ITEM_MATERIAL_DESC_PROP: "materialDescription",
    PLANNED_DELIVERY_DATE_PROP: "plannedDeliveryDate",
    PROCESS_STATUS_CODE_PROP  : "processStatus_code",
    EXECUTION_STATUS_CODE_PROP: "executionStatus_code",
    PO_TO_PO_ITEM_NAV_PATH    : "purchaseOrderItemTPs/purchaseOrderItem",
    LAST_UPDATE_PATH          : "/lastUpdatedOn",
    FOCUS_GROUP_PATH          : "/focusGroup",
    PURCHASE_ORDER_ID_PATH    : "/purchaseOrderId",

    PROCESS_STATUS_CODE: {
      AS_PLANNED  : "AS_PLANNED",
      EARLY       : "EARLY",
      LATE        : "LATE",
      OVERDUE     : "OVERDUE",
      DELAYED     : "DELAYED",
    },

    EXECUTION_STATUS_CODE: {
      NOT_STARTED : "NOT_STARTED",
      IN_TRANSIT  : "IN_TRANSIT",
      COMPLETED    : "COMPLETED",
    },

    DOCUMENT_FLOW_GROUP: {
      PURCHASE_ORDER        : 1,
      PURCHASE_ORDER_ITEM   : 2,
      INDOUND_DELIVERY_ITEM : 3,
      INDOUND_DELIVERY      : 4,
      SHIPMENT              : 5,
      RESOURCE              : 6,
    },

    ROUTES_NAME: {
      PURCHASE_ORDER        : "PurchaseOrderDetails",
      PURCHASE_ORDER_ITEM   : "PurchaseOrderItemDetails",
      INDOUND_DELIVERY_ITEM : "DeliveryItemDetails",
    },

    MILESTONE_PROCESS_NAME: {
      CREATED           : "purchaseOrderItemCreated",
      RECEIPT           : "purchaseOrderItemReceipt",
      CONFIRMED         : "purchaseOrderItemConfirmed",
      DELETED           : "purchaseOrderItemDeleted",
      DELIVERY_CREATED  : "inboundDeliveryItemCreated",
      DELIVERY_RECEIPT  : "inboundDeliveryItemReceipt",
      DELIVERY_COMPLETE : "inboundDeliveryItemCompleted",
    },

    DEFAULT_VARIANT_FILTER: "STANDARD",

    TIMELINE_EVENT_TYPE: {
      ARRIVAL       : "Arrival",
      DEPARTURE     : "Departure",
      LOADING_START : "LoadingStart",
      LOADING_END   : "LoadingEnd",
      CHECK_IN      : "CheckIn",
    },

    EVENT_STATUS_CODE: {
      DELAYED         : "DELAYED",
      OVERDUE         : "OVERDUE",
      REPORTED        : "REPORTED",
      EARLY_REPORTED  : "EARLY_REPORTED",
      LATE_REPORTED   : "LATE_REPORTED",
      PLANNED         : "PLANNED",
      UNPLANNED       : "UNPLANNED",
      LATE            : "LATE",
      IN_TRANSIT      : "IN_TRANSIT",
    },

    EVENT_TYPE: {
      LOCATION_UPDATE: "LocationUpdate",
      DELAY: "Delay",
    },

    LOCATION_TYPE_CODE: {
      PLANT             : "Plant",
      SUPPLIER          : "Supplier",
      CUSTOMER          : "Customer",
      SHIPPING_POINT    : "ShippingPoint",
      LOGISTIC_LOCATION : "LogisticLocation",
    },

    ROUTE_WIDTH: {
      DEFAULT: 4,
    },

    DEVICE_TYPE: {
      PHONE  : "Phone",
      TABLET : "Tablet",
      DESKTOP: "Desktop",
    },

    REF_BUSINESS_DOCUMENTS_TYPE:{
      VP: "VP",
    },

  });
});
