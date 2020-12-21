sap.ui.define([
], function () {
  "use strict";

  var TransportationMode = {};

  /**
   * Enum type for the execution status
   * @enum {string}
   * @public
   */
  TransportationMode.Type = {
    SEA: "01",
    RAIL: "02",
    ROAD: "03",
    AIR: "04",
    MAIL: "05",
    NOT_SPECIFIED: "00",
    NOT_APPLICABLE: "09",
  };

  /**
   * Get the transportion mode icon
   *
   * @param {string} type The transportion mode
   * @returns {string} The sap icon
   */
  TransportationMode.getIcon = function (type) {
    switch (type) {
      case TransportationMode.Type.SEA:
        return "sap-icon://BusinessSuiteInAppSymbols/icon-ship";
      case TransportationMode.Type.RAIL:
        return "sap-icon://cargo-train";
      case TransportationMode.Type.ROAD:
        return "sap-icon://shipping-status";
      case TransportationMode.Type.AIR:
        return "sap-icon://flight";
      case TransportationMode.Type.MAIL:
        return "sap-icon://email";
      default:
        return "";
    }
  };

  return Object.freeze(TransportationMode);
});
