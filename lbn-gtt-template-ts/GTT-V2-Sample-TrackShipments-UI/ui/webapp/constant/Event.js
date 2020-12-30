sap.ui.define(
  [
    "sap/ui/core/ValueState",
    "sap/ui/core/IconColor",
    "sap/m/ValueColor",
  ], function (
    ValueState,
    IconColor,
    ValueColor
  ) {
    "use strict";

    var Event = {};

    /**
     * Enum type for the event type
     *
     * @readonly
     * @enum {string}
    */
    Event.Type = {
      DEPARTURE: "Departure",
      ARRIVAL: "Arrival",
      POD: "POD",
      DELIVERYITEM_POD: "DeliveryItemPOD",
      GATE_IN_START: "GateInStart",
      GATE_IN_END: "GateInEnd",
      GATE_OUT_START: "GateOutStart",
      GATE_OUT_END: "GateOutEND",
      GATE_IN: "GateIn",
      GATE_OUT: "GateOut",
      LOADING_START: "LoadingStart",
      LOADING_END: "LoadingEnd",
      UNLOADING_START: "UnLoadingStart",
      UNLOADING_END: "UnLoadingEnd",
      EMPTY: "Empty",
      STUFFING: "Stuffing",
      STRIPPING: "Stripping",
      PROOF_OF_PICK_UP: "POPU",
      RETURN: "Return",
      GOODS_ISSUED: "GoodsIssued",
      PICKING: "Picking",
      PACKING: "Packing",
      LOCATION_UPDATE: "LocationUpdate",
      DELAY: "Delay",
    };

    // eslint-disable-next-line complexity
    Event.Type.getIcon = function (type) {
      switch(type) {
        case Event.Type.DEPARTURE:
          return "sap-icon://journey-depart";
        case Event.Type.ARRIVAL:
          return "sap-icon://journey-arrive";
        case Event.Type.POD:
        case Event.Type.DELIVERYITEM_POD:
          return "sap-icon://receipt";
        case Event.Type.GATE_IN_START:
          return "sap-icon://inbox";
        case Event.Type.GATE_IN_END:
          return "sap-icon://bbyd-active-sales";
        case Event.Type.GATE_OUT_START:
          return "sap-icon://outbox";
        case Event.Type.GATE_OUT_END:
          return "sap-icon://bbyd-active-sales";
        case Event.Type.GATE_IN:
          return "sap-icon://visits";
        case Event.Type.GATE_OUT:
          return "sap-icon://offsite-work";
        case Event.Type.LOADING_START:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-truck-load";
        case Event.Type.LOADING_END:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck";
        case Event.Type.UNLOADING_START:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-truck-unload";
        case Event.Type.UNLOADING_END:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck-empty";
        case Event.Type.EMPTY:
          return "sap-icon://action";
        case Event.Type.STUFFING:
          return "sap-icon://cause";
        case Event.Type.STRIPPING:
          return "sap-icon://split";
        case Event.Type.RETURN:
          return "sap-icon://undo";
        case Event.Type.PROOF_OF_PICK_UP:
          return "sap-icon://activity-2";
        case Event.Type.GOODS_ISSUED:
          return "sap-icon://activity-2";
        case Event.Type.PICKING:
          return "sap-icon://SAP-icons-TNT/internal-block-diagram";
        case Event.Type.PACKING:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-products";
      }

      return "sap-icon://action";
    };

    /**
     * Enum type for the event status
     *
     * @readonly
     * @enum {string}
    */
    Event.Status = {};

    /**
     * Enum type for the event status
     *
     * @readonly
     * @enum {string}
    */
    Event.Status.Type = {
      "PLANNED": "PLANNED",
      "UNPLANNED": "UNPLANNED",
      "OVERDUE": "OVERDUE",
      "REPORTED_EARLY": "EARLY_REPORTED",
      "REPORTED_ON_TIME": "REPORTED",
      "REPORTED_LATE": "LATE_REPORTED",
      "REPORTED": "REPORTED",
      "DELAYED": "DELAYED",
    };

    Event.Status.getColor = function (status) {
      switch (status) {
        case Event.Status.Type.PLANNED:
        case Event.Status.Type.UNPLANNED:
          return ValueColor.Neutral;
        case Event.Status.Type.OVERDUE:
          return ValueColor.Critical;
        case Event.Status.Type.REPORTED:
          return ValueColor.Good;
        case Event.Status.Type.DELAYED:
          return ValueColor.Error;
        default:
          return ValueColor.Neutral;
      }
    };

    Event.Status.getState = function (status) {
      switch (status) {
        case Event.Status.Type.DELAYED:
          return ValueState.Error;
        case Event.Status.Type.PLANNED:
        case Event.Status.Type.UNPLANNED:
          return ValueState.None;
        case Event.Status.Type.OVERDUE:
          return ValueState.Warning;
        default:
          return ValueState.Success;
      }
    };

    Event.Status.getIconColor = function (status) {
      switch (status) {
        case Event.Status.Type.PLANNED:
        case Event.Status.Type.UNPLANNED:
          return IconColor.Neutral;
        case Event.Status.Type.REPORTED_ON_TIME:
          return IconColor.Positive;
        default:
          return IconColor.Critical;
      }
    };

    /**
     * Check if is an actual event status
     *
     * @param {Event.Status.Type} status Event status type
     * @returns {boolean} `true` if the event is an actual one
     */
    Event.Status.isActual = function (status) {
      return (
        status === Event.Status.Type.REPORTED ||
        status === Event.Status.Type.REPORTED_EARLY ||
        status === Event.Status.Type.REPORTED_LATE ||
        status === Event.Status.Type.REPORTED_ON_TIME ||
        status === Event.Status.Type.UNPLANNED
      );
    };

    Event.Status.hasNoHistoricalReporting = function (status) {
      return status === Event.Status.Type.PLANNED
        || status === Event.Status.Type.OVERDUE
        || status === Event.Status.Type.DELAYED;
    };

    return Object.freeze(Event);
  }
);
