sap.ui.define([
  "com/sap/gtt/app/sample/sst/controller/TrackingTimeline.controller",
  "sap/ui/core/Fragment",
], function (
  TrackingTimeline,
  Fragment
) {
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

  QUnit.module("com.sap.gtt.app.sst.controller.TrackingTimeline", {
    beforeEach: function () {
      this.controller = new TrackingTimeline();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("revealLocationOnMap", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeSelectedItem = {};
    var fakeSelectedEventModel = {};
    var fakePosition = {
      longitude: 0,
      latitude: 0,
    };
    var fakeSelectedEvent = {
      location: fakePosition,
    };

    stub(fakeSelectedItem, "getBindingContext").returns(fakeSelectedEventModel);
    stub(fakeSelectedItem, "getId").returns("id1");
    stub(fakeSelectedEventModel, "getObject").returns(fakeSelectedEvent);

    var fakeMap = {};
    stub(fakeMap, "zoomToGeoPosition");
    stub(fakeMap, "getZoomlevel");
    stub(fakeMap, "data").withArgs("currentId").returns("id1");
    stub(controller, "createId");
    stub(Fragment, "byId").returns(fakeMap);

    // Act
    controller.revealLocationOnMap(fakeSelectedItem);

    // Assert
    assert.ok(fakeMap.zoomToGeoPosition.calledOnce, "The map is zoomed to the position");
  });

  QUnit.test("openReportingHistoryPopover", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    var fakeSource = {};
    var fakeHistoricalEvents = [];
    var fakePopover = {};
    var fakeProps = {};
    stub(fakePopover, "openBy");
    stub(fakePopover, "setModel");
    stub(fakePopover, "getModel").withArgs("props").returns(fakeProps);
    var fakePromise = Promise.resolve(fakePopover);
    stub(Fragment, "load").returns(fakePromise);
    var fakeView = controller.getView();
    stub(fakeView, "addDependent");

    // Act
    controller.openReportingHistoryPopover(fakeSource, fakeHistoricalEvents);

    // Assert
    fakePromise.then(function (popover) {
      assert.ok(popover.openBy.calledOnce, "Reporting history popover is opened");
      done();
    });
  });

  QUnit.test("getRouteTooltip - from id to to id", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeView = controller.getView();
    var fakeBindingContext = {};
    var fakeShipment = {
      departureLocationId: "ID 1",
      arrivalLocationId: "ID 2",
      departureLocation: null,
      arrivalLocation: null,
    };
    stub(fakeBindingContext, "getObject").returns(fakeShipment);
    stub(fakeView, "getBindingContext").returns(fakeBindingContext);
    stub(controller, "getText");

    // Act
    controller.getRouteTooltip();

    // Assert
    assert.ok(controller.getText.calledWith("routeTooltip", ["ID 1", "ID 2"]), "The tooltip is right");
  });

  QUnit.test("getRouteTooltip - <Undefined> to <Undefined>", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeView = controller.getView();
    var fakeBindingContext = {};
    var fakeShipment = {
      departureLocationId: null,
      arrivalLocationId: null,
      departureLocation: null,
      arrivalLocation: null,
    };
    stub(fakeBindingContext, "getObject").returns(fakeShipment);
    stub(fakeView, "getBindingContext").returns(fakeBindingContext);
    stub(controller, "getText")
      .withArgs("locationUndefined").returns("<Undefined>");

    // Act
    controller.getRouteTooltip();

    // Assert
    assert.deepEqual(controller.getText.thirdCall.args, ["routeTooltip", ["<Undefined>", "<Undefined>"]], "The tooltip is right");
  });
});
