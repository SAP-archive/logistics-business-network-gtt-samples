sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/deliveryItem/TrackingTimeline.controller",
], function (TrackingTimeline) {
  "use strict";

  var sandbox = sinon.createSandbox();

  function stub(object, method, func) {
    if (!(method in object)) {
      object[method] = function () {};
    }

    var stubbed = sandbox.stub(object, method);

    if (typeof func === "function") {
      return stubbed.callsFake(func);
    }

    return stubbed;
  }

  QUnit.module("com.sap.gtt.app.pof.controller.deliveryItem.TrackingTimeline", {
    beforeEach: function () {
      this.oTrackingTimeline = new TrackingTimeline();
      this.oTrackingTimeline._oMap = {};
      this.oTrackingTimeline.oMapModel = {};
    },
    afterEach: function () {
      sandbox.restore();
      this.oTrackingTimeline.destroy();
    },
  });


  QUnit.test("onTimelineItemSelect - select timeline event", function (assert) {
    // Arrange
    var oFakeEvent = {},
      oFakeParameter = {},
      oFakeCtx = {},
      oFakeData = {};

    stub(oFakeEvent, "getParameter").returns(oFakeParameter);
    stub(oFakeParameter, "getBindingContext").returns(oFakeCtx);
    stub(this.oTrackingTimeline, "_getSelectedStop").returns();
    stub(oFakeCtx, "getObject").returns(oFakeData);

    // Act
    var oResult = this.oTrackingTimeline.onTimelineItemSelect(oFakeEvent);

    // Assert
    assert.ok(!oResult, "The function doesn't return anything.");
  });

  QUnit.test("onTimelineItemSelect - select timeline event that exists on the Map", function (assert) {
    // Arrange
    var oFakeEvent = {},
      oFakeParameter = {},
      oFakeCtx = {};
    var aFakeStops = [
      {plannedEventId: "123", eventId: "123", longitude: 55},
    ];

    var oController = this.oTrackingTimeline;

    stub(oController._oMap, "setCenterPosition").returns();
    stub(oController, "_getSelectedStop").returns({});
    stub(oController.oMapModel, "getProperty").returns(aFakeStops);

    stub(oFakeEvent, "getParameter").returns(oFakeParameter);
    stub(oFakeParameter, "getBindingContext").returns(oFakeCtx);
    stub(oFakeCtx, "getObject").returns({});

    // Act
    var oResult = oController.onTimelineItemSelect(oFakeEvent);

    // Assert
    assert.ok(!oResult, "The function doesn't return anything.");
    assert.ok(oController._oMap.setCenterPosition.calledOnce, "'setCenterPosition' was caalled once");
  });

  QUnit.test("_getSelectedStop - return selected stop: no plannedEventId", function (assert) {
    // Arrange
    var oController = this.oTrackingTimeline;
    var oFakeData = {
      plannedEventId: null,
      actualEventId: "123",
    };
    var aFakeStops = [
      {plannedEventId: "123", eventId: "123", longitude: 55},
    ];

    stub(oController.oMapModel, "getProperty").returns(aFakeStops);

    // Act
    var oSelectedStop = oController._getSelectedStop(oFakeData);

    // Assert
    assert.ok(!!oSelectedStop, "The function returns anything.");
    assert.ok(oSelectedStop.longitude === 55, "Check longitude of event");
  });

  QUnit.test("_getSelectedStop - return selected stop: has plannedEventId and actualEventId", function (assert) {
    // Arrange
    var oController = this.oTrackingTimeline;
    var oFakeData = {
      plannedEventId: "890",
      actualEventId: "123",
    };
    var aFakeStops = [
      {plannedEventId: "123", eventId: "123", longitude: 55},
      {plannedEventId: "123", eventId: "890", longitude: 48},
    ];

    stub(oController.oMapModel, "getProperty").returns(aFakeStops);

    // Act
    var oSelectedStop = oController._getSelectedStop(oFakeData);

    // Assert
    assert.ok(!!oSelectedStop, "The function returns anything.");
    assert.ok(oSelectedStop.longitude === 48, "Check longitude of event");
  });

  QUnit.test("_getSelectedStop - return selected stop: has plannedEventId and doesn't have actualEventId", function (assert) {
    // Arrange
    var oController = this.oTrackingTimeline;
    var oFakeData = {
      plannedEventId: "654",
      actualEventId: null,
    };
    var aFakeStops = [
      {plannedEventId: "123", eventId: "123", longitude: 55},
      {plannedEventId: "123", eventId: "890", longitude: 48},
      {plannedEventId: "654", eventId: "345", longitude: 90},
    ];

    stub(oController.oMapModel, "getProperty").returns(aFakeStops);

    // Act
    var oSelectedStop = oController._getSelectedStop(oFakeData);

    // Assert
    assert.ok(!!oSelectedStop, "The function returns anything.");
    assert.ok(oSelectedStop.eventId === "345", "Check eventId of event");
  });
});
