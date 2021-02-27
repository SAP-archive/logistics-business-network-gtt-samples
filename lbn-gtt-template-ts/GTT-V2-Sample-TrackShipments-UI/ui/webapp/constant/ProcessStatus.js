sap.ui.define(
  [
    "sap/ui/core/ValueState",
    "sap/ui/core/MessageType",
  ], function (
    ValueState,
    MessageType
  ) {
    "use strict";

    /**
     * Enum type for the process status
     * @enum {string}
     * @public
     */
    var ProcessStatus = {};

    /**
     * Enum type for the process status
     *
     * @readonly
     * @enum {string}
    */
    var Type = {
      AS_PLANNED: "AS_PLANNED",
      DELAYED: "DELAYED",
      EARLY: "EARLY",
      LATE: "LATE",
      OVERDUE: "OVERDUE",
    };

    ProcessStatus.getState = function (status) {
      switch(status) {
        case Type.EARLY:
        case Type.LATE:
        case Type.OVERDUE:
          return ValueState.Warning;
        case Type.DELAYED:
          return ValueState.Error;
        default:
          return ValueState.Information;
      }
    };

    ProcessStatus.getListRowState = function (status) {
      switch(status) {
        case Type.EARLY:
        case Type.LATE:
        case Type.OVERDUE:
          return MessageType.Warning;
        case Type.DELAYED:
          return MessageType.Error;
        default:
          return MessageType.None;
      }
    };

    return Object.freeze(ProcessStatus);
  }
);
