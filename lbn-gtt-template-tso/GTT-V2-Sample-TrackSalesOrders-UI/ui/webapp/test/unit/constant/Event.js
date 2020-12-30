sap.ui.define([
  "sap/ui/core/IconColor",
  "sap/base/strings/formatMessage",
  "com/sap/gtt/app/sample/sof/constant/Event",
], function (IconColor, formatMessage, Event) {
  "use strict";

  var sandbox = sinon.createSandbox();

  QUnit.module("com.sap.gtt.app.sof.constant.Event", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("getIcon", function (assert) {
    var events = [
      { type: Event.Type.DEPARTURE, icon: "sap-icon://journey-depart" },
      { type: Event.Type.ARRIVAL, icon: "sap-icon://journey-arrive" },
      { type: Event.Type.POD, icon: "sap-icon://receipt" },
      { type: Event.Type.DELIVERYITEM_POD, icon: "sap-icon://receipt" },
      { type: Event.Type.GATE_IN_START, icon: "sap-icon://inbox" },
      { type: Event.Type.GATE_IN_END, icon: "sap-icon://bbyd-active-sales" },
      { type: Event.Type.GATE_OUT_START, icon: "sap-icon://outbox" },
      { type: Event.Type.GATE_OUT_END, icon: "sap-icon://bbyd-active-sales" },
      { type: Event.Type.GATE_IN, icon: "sap-icon://visits" },
      { type: Event.Type.GATE_OUT, icon: "sap-icon://offsite-work" },
      { type: Event.Type.LOADING_START, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-truck-load" },
      { type: Event.Type.LOADING_END, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck" },
      { type: Event.Type.UNLOADING_START, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-truck-unload" },
      { type: Event.Type.UNLOADING_END, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck-empty" },
      { type: Event.Type.EMPTY, icon: "sap-icon://action" },
      { type: Event.Type.STUFFING, icon: "sap-icon://cause" },
      { type: Event.Type.STRIPPING, icon: "sap-icon://split" },
      { type: Event.Type.PROOF_OF_PICK_UP, icon: "sap-icon://activity-2" },
      { type: Event.Type.RETURN, icon: "sap-icon://undo" },
      { type: Event.Type.GOODS_ISSUED, icon: "sap-icon://activity-2" },
      { type: Event.Type.PICKING, icon: "sap-icon://SAP-icons-TNT/internal-block-diagram" },
      { type: Event.Type.PACKING, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-products" },
      { type: undefined, icon: "sap-icon://action" },
    ];

    // Assert
    events.forEach(function (event) {
      assert.equal(
        Event.Type.getIcon(event.type),
        event.icon,
        formatMessage("The icon of {0} event is right", [event.type])
      );
    });
  });

  QUnit.test("getIconColor", function (assert) {
    var statuses = [
      { type: Event.Status.Type.PLANNED, iconColor: IconColor.Neutral },
      { type: Event.Status.Type.REPORTED_ON_TIME, iconColor: IconColor.Positive },
      { type: undefined, iconColor: IconColor.Critical },
    ];

    // Assert
    statuses.forEach(function (status) {
      assert.equal(
        Event.Status.getIconColor(status.type),
        status.iconColor,
        formatMessage("The icon color of {0} status is right", [status.type])
      );
    });
  });
});
