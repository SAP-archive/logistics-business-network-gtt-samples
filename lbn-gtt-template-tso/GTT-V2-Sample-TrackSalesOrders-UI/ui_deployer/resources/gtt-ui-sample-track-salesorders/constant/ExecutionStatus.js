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

  return ExecutionStatus;
});
