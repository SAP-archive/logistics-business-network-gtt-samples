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
        case Type.LATE:
          return ValueState.Warning;
        case Type.DELAYED:
        case Type.OVERDUE:
          return ValueState.Error;
        default:
          return ValueState.Success;
      }
    };

    ProcessStatus.getListRowState = function (status) {
      switch(status) {
        case Type.LATE:
          return MessageType.Warning;
        case Type.DELAYED:
        case Type.OVERDUE:
          return MessageType.Error;
        default:
          return MessageType.None;
      }
    };

    return Object.freeze(ProcessStatus);
  }
);
