sap.ui.define(
  [
    "sap/ui/core/ValueState",
    "sap/m/ValueColor",
  ], function (
    ValueState,
    ValueColor
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

    ProcessStatus.getColor = function (status) {
      switch(status) {
        case Type.EARLY:
        case Type.AS_PLANNED:
          return ValueColor.Good;
        case Type.LATE:
          return ValueColor.Critical;
        case Type.OVERDUE:
        case Type.DELAYED:
          return ValueColor.Error;
        default:
          return ValueColor.Good;
      }
    };

    ProcessStatus.getIcon = function (status) {
      switch(status) {
        case Type.EARLY:
          return "sap-icon://past";
        case Type.AS_PLANNED:
          return "sap-icon://fob-watch";
        case Type.LATE:
          return "sap-icon://lateness";
        case Type.DELAYED:
          return "sap-icon://time-overtime";
        case Type.OVERDUE:
          return "sap-icon://pending";
        default:
          return "sap-icon://process";
      }
    };

    ProcessStatus.getState = function (status) {
      switch(status) {
        case Type.EARLY:
        case Type.AS_PLANNED:
          return ValueState.Success;
        case Type.LATE:
          return ValueState.Warning;
        case Type.DELAYED:
        case Type.OVERDUE:
          return ValueState.Error;
        default:
          return ValueState.None;
      }
    };

    return Object.freeze(ProcessStatus);
  }
);
