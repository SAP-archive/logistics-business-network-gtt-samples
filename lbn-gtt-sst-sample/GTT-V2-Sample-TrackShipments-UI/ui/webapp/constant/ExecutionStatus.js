sap.ui.define([
  "sap/ui/core/ValueState",
], function (ValueState) {
  "use strict";

  var ExecutionStatus = {};

  /**
   * Enum type for the execution status
   * @enum {string}
   * @public
   */
  ExecutionStatus.Type = {
    NOT_STARTED: "NOT_STARTED",
    IN_TRANSIT: "IN_TRANSIT",
    COMPLETED: "COMPLETED",
  };

  /**
   * Get the execution status state
   *
   * @param {string} status The execution status
   * @returns {sap.ui.core.ValueState} The value state
   */
  ExecutionStatus.getState = function (status) {
    switch (status) {
      case ExecutionStatus.Type.COMPLETED:
        return ValueState.Success;
      case ExecutionStatus.Type.NOT_STARTED:
        return ValueState.None;
      case ExecutionStatus.Type.IN_TRANSIT:
        return ValueState.Information;
    }

    return ValueState.Information;
  };

  /**
   * Get the execution status icon
   *
   * @param {string} type The execution status
   * @returns {string} The sap icon
   */
  ExecutionStatus.getIcon = function (type) {
    switch (type) {
      case ExecutionStatus.Type.NOT_STARTED:
        return "sap-icon://status-inactive";
      case ExecutionStatus.Type.IN_TRANSIT:
        return "sap-icon://status-in-process";
      case ExecutionStatus.Type.COMPLETED:
        return "sap-icon://status-completed";
      default:
        return "sap-icon://status-inactive";
    }
  };

  return Object.freeze(ExecutionStatus);
});
