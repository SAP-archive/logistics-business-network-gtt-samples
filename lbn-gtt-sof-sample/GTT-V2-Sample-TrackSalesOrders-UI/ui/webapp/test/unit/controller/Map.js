sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/Map.controller",
  "com/sap/gtt/app/sample/sof/util/ServiceUtils",
  "com/sap/gtt/app/sample/sof/util/AsyncUtils",
  "com/sap/gtt/app/sample/sof/util/RestClient",
  "sap/ui/core/Fragment",
], function (Map, ServiceUtils, AsyncUtils, RestClient, Fragment) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.Map", {
    beforeEach: function () {
      this.controller = new Map();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("showRouteDetail", function (assert) {
    var controller = this.controller;

    // Arrange
    stub(controller, "handleRouteHighlight");
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(fakeModel, "getProperty")
      .withArgs("/selectedRoute/groupId").returns("group")
      .withArgs("/deliveryItemId").returns("deliveryItemId");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(controller, "showSideContent");

    var fakeEvent = {};
    var fakeSource = {};
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getObject");
    stub(fakeBindingContext, "getProperty")
      .withArgs("groupId").returns("groupId2");
    stub(fakeSource, "getBindingContext").withArgs("map").returns(fakeBindingContext);
    stub(fakeEvent, "getSource").returns(fakeSource);
    stub(controller, "updateParametersForEvents");
    stub(controller, "updateEventsOnSideContent");

    // Act
    controller.showRouteDetail(fakeEvent);

    // Assert
    assert.ok(controller.updateEventsOnSideContent.calledAfter(controller.updateParametersForEvents), "The side content will open");
  });

  QUnit.test("updateEventsOnSideContent - success", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    stub(controller, "handleRouteHighlight");
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(ServiceUtils, "getDataSource").withArgs("restService").returns({
      uri: "rest/v1",
    });
    stub(ServiceUtils, "getUrl");
    stub(AsyncUtils, "finally");

    var fakePromise = Promise.resolve("fakeData");
    stub(RestClient, "get").returns(fakePromise);

    // Act
    controller.updateEventsOnSideContent();

    // Assert
    fakePromise.then(function (data) {
      assert.ok(fakeModel.setProperty.calledWith("/selectedEvents", "fakeData"), "The events are set");
      done();
    });
  });

  QUnit.test("updateEventsOnSideContent - error", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    stub(controller, "handleRouteHighlight");
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(ServiceUtils, "getDataSource").withArgs("restService").returns({
      uri: "rest/v1",
    });
    stub(ServiceUtils, "getUrl");
    stub(AsyncUtils, "finally");

    var fakePromise = Promise.reject("fakeError");
    stub(RestClient, "get").returns(fakePromise);
    stub(controller, "handleServerError");

    // Act
    controller.updateEventsOnSideContent();

    // Assert
    fakePromise.then(function () {
    },
    function (error) {
      assert.ok(controller.handleServerError.calledWith("fakeError"), "The error will be handled");
      done();
    });
  });

  QUnit.test("updateParametersForEvents", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty")
      .withArgs("groupId").returns("fakeGroupId")
      .withArgs("firstPlannedEventId").returns("fakeEventId")
      .withArgs("altKey").returns("fakeAltKey");

    // Act
    var fakeParams = {};
    controller.updateParametersForEvents(fakeParams, fakeBindingContext);

    // Assert
    assert.deepEqual(fakeParams, {
      eventMatchKey: "fakeGroupId",
      plannedEventId: "fakeEventId",
      altKey: "fakeAltKey",
    }, "The parameters are got");
  });

  QUnit.test("showHistoricalReporting", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(controller, "setControlFocus");

    var fakeEvent = {};
    var fakeSource = {};
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getPath");
    stub(fakeBindingContext, "getProperty");
    stub(fakeSource, "getBindingContext").withArgs("map").returns(fakeBindingContext);
    stub(fakeEvent, "getSource").returns(fakeSource);
    stub(controller, "createId").withArgs("detailsPanel").returns("fakeControl");
    stub(controller, "updateHistoricalEvents");

    var fakePanel = {};
    stub(fakePanel, "bindElement");
    stub(Fragment, "byId").withArgs("fakeControl", "panel").returns(fakePanel);

    // Act
    controller.showHistoricalReporting(fakeEvent);

    // Assert
    assert.ok(controller.updateHistoricalEvents.calledOnce, "The events will be updated");
  });

  QUnit.test("showEventsPanel", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller, "setControlFocus");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);

    // Act
    controller.showEventsPanel();

    // Assert
    assert.ok(controller.setControlFocus.calledWith("closeButton", "eventsPanel"), "Close button will be focused");
  });

  QUnit.test("setControlFocus - is fragment control", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    stub(controller, "createId");
    var fakeControl = {};
    stub(fakeControl, "focus");
    stub(Fragment, "byId").returns(fakeControl);

    // Act
    controller.setControlFocus("controlId", "fragmentId");

    // Assert
    setTimeout(function () {
      assert.ok(fakeControl.focus.calledOnce, "The control is focused");
      done();
    }, 100);
  });

  QUnit.test("setControlFocus", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    var fakeControl = {};
    stub(fakeControl, "focus");
    stub(controller, "byId").returns(fakeControl);

    // Act
    controller.setControlFocus("controlId");

    // Assert
    setTimeout(function () {
      assert.ok(fakeControl.focus.calledOnce, "The control is focused");
      done();
    }, 100);
  });

  QUnit.test("handleRouteHighlight, toggleHighlightRouteWidth", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeEvent = {};
    stub(fakeEvent, "getSource");
    stub(controller, "cancelRouteHighlight");
    stub(controller, "toggleHighlightRouteWidth");

    // Act
    controller.handleRouteHighlight(fakeEvent);

    // Assert
    assert.ok(controller.toggleHighlightRouteWidth.calledAfter(controller.cancelRouteHighlight), "The route highlight state will change");
  });

  QUnit.test("cancelRouteHighlight", function (assert) {
    var controller = this.controller;

    // Arrange
    controller.selectedRoute = {};
    stub(controller, "toggleHighlightRouteWidth");

    // Act
    controller.cancelRouteHighlight();

    // Assert
    assert.ok(controller.toggleHighlightRouteWidth.calledWith(controller.RouteWidth.NORMAL), "The route will be highlighted");
  });

  QUnit.test("toggleHighlightRouteWidth", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    var fakeSelectRoute = {};
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty").withArgs("groupId").returns("groupId");
    stub(fakeSelectRoute, "getBindingContext").withArgs("map").returns(fakeBindingContext);
    controller.selectedRoute = fakeSelectRoute;
    stub(controller, "getRouteBindingPath")
      .withArgs("groupId", "planned").returns("path1")
      .withArgs("groupId", "actual").returns("path2");

    // Act
    controller.toggleHighlightRouteWidth(controller.RouteWidth.BOLD);

    // Assert
    assert.ok(fakeModel.setProperty.calledTwice, "The route width is updated");
  });

  QUnit.test("getRouteBindingPath - planned", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(fakeModel, "getProperty").withArgs("/plannedRoutes").returns([{groupId: "groupId2"}]);

    // Act
    var path = controller.getRouteBindingPath("groupId2", "planned");

    // Assert
    assert.ok(path === "/0", "The path is got");
  });

  QUnit.test("getRouteBindingPath - actual", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(controller, "getModel").withArgs("map").returns(fakeModel);
    stub(fakeModel, "getProperty").withArgs("/actualRoutes").returns([{groupId: "groupId1"}]);

    // Act
    var path = controller.getRouteBindingPath("groupId1", "actual");

    // Assert
    assert.ok(path === "/0", "The path is got");
  });

  QUnit.test("showSideContent", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeControl = {};
    stub(fakeControl, "setShowSideContent");
    stub(fakeControl, "fireBreakpointChanged");
    stub(fakeControl, "getCurrentBreakpoint").returns("M");
    stub(controller, "setControlFocus");
    stub(controller, "byId").withArgs("dynamicSideContent").returns(fakeControl);

    // Act
    controller.showSideContent();

    // Assert
    assert.ok(fakeControl.fireBreakpointChanged.calledWith({currentBreakpoint: "M"}), "The side content is updated");
    assert.ok(controller.setControlFocus.calledWith("closeButton", "eventsPanel"), "The close button will be focused");
  });

  QUnit.test("onBreakPointChanged", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeEvent = {};
    stub(fakeEvent, "getParameter").withArgs("currentBreakpoint").returns("M");
    var fakeControl = {};
    stub(fakeControl, "setShowMainContent");
    stub(fakeControl, "setShowSideContent");
    stub(fakeControl, "getShowSideContent").returns(true);
    stub(controller, "byId").withArgs("dynamicSideContent").returns(fakeControl);

    // Act
    controller.onBreakPointChanged(fakeEvent);

    // Assert
    assert.ok(fakeControl.setShowMainContent.calledWith(false), "Main content is not visible");
    assert.ok(fakeControl.setShowSideContent.calledWith(true), "Side content is visible");
  });

  QUnit.test("showImpactAnalysis - no dialog", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    var fakeModel = {};
    var fakePlannedEventId = "fakePlannedEventId";
    stub(controller, "getModel").withArgs("impactAnalysis").returns(fakeModel);
    stub(fakeModel, "setProperty");
    var fakeDialog = {};
    stub(fakeDialog, "open");
    var fakePromise = Promise.resolve(fakeDialog);
    stub(Fragment, "load").returns(fakePromise);
    var fakeView = controller.getView();
    stub(fakeView, "addDependent");

    // Act
    controller.showImpactAnalysis(fakePlannedEventId);

    fakePromise.then(function (dialog) {
      // Assert
      assert.ok(fakeDialog.open.calledOnce, "Dialog will open");
      done();
    });
  });

  QUnit.test("showImpactAnalysis - has dialog", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    var fakePlannedEventId = "fakePlannedEventId";
    stub(controller, "getModel").withArgs("impactAnalysis").returns(fakeModel);
    stub(fakeModel, "setProperty");

    // Arrange
    controller.impactAnalysis = {};
    stub(controller.impactAnalysis, "open");

    // Act
    controller.showImpactAnalysis(fakePlannedEventId);

    // Assert
    assert.ok(controller.impactAnalysis.open.calledOnce, "Impact analysis dialog is opened");
  });

  QUnit.test("hideImpactAnalysis", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeModel = {};
    stub(controller, "getModel").withArgs("impactAnalysis").returns(fakeModel);
    stub(fakeModel, "setProperty");
    controller.impactAnalysis = {};
    stub(controller.impactAnalysis, "close");

    // Act
    controller.hideImpactAnalysis();

    // Assert
    assert.ok(controller.impactAnalysis.close.calledOnce, "Impact analysis dialog is closed");
    assert.deepEqual(fakeModel.setProperty.args[0], ["/nodes", []], "Nodes have been emptied");
    assert.deepEqual(fakeModel.setProperty.args[1], ["/lines", []], "Lines have been emptied");
  });

  QUnit.test("initImpactAnalysis - success", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    var fakeImpactAnalysisModel = {};
    var fakeMapModel = {};
    var fakePlannedEventId = "fakePlannedEventId";
    var fakeDeliveryItemId = "fakeDeliveryItemId";

    stub(controller, "getModel")
      .withArgs("impactAnalysis").returns(fakeImpactAnalysisModel)
      .withArgs("map").returns(fakeMapModel);
    stub(fakeImpactAnalysisModel, "getProperty").withArgs("/plannedEventId").returns(fakePlannedEventId);
    stub(fakeMapModel, "getProperty").withArgs("/deliveryItemId").returns(fakeDeliveryItemId);
    stub(fakeImpactAnalysisModel, "setProperty");
    stub(ServiceUtils, "getDataSource").withArgs("restService").returns({
      uri: "rest/v1",
    });
    stub(ServiceUtils, "getUrl");
    stub(AsyncUtils, "finally");

    var fakePromise = Promise.resolve({
      nodes: [],
      lines: [],
    });
    stub(RestClient, "get").returns(fakePromise);

    // Act
    controller.initImpactAnalysis();

    // Assert
    fakePromise.then(function (data) {
      assert.deepEqual(fakeImpactAnalysisModel.setProperty.args[0], ["/isLoaded", false], "Impact analysis is loading");
      assert.deepEqual(fakeImpactAnalysisModel.setProperty.args[1], ["/nodes", data.nodes], "Nodes have been set");
      assert.deepEqual(fakeImpactAnalysisModel.setProperty.args[2], ["/lines", data.lines], "Lines have been set");
      assert.deepEqual(fakeImpactAnalysisModel.setProperty.args[3], ["/isLoaded", true], "Impact analysis has been loaded");
      done();
    });
  });

  QUnit.test("updateImpactAnalysis - success", function (assert) {
    var controller = this.controller;
    var done = assert.async();

    // Arrange
    var fakeModel = {};
    var fakeModelData = {
      nodes: [],
      lines: [],
    };
    var fakeSource = {};
    var fakeKey = "fakeKey";
    var fakeId = "fakeId";
    var fakeTrackingIdType = "fakeTrackingIdType";
    var fakePromise = Promise.resolve({
      nodes: [],
    });

    stub(controller, "getModel").withArgs("impactAnalysis").returns(fakeModel);
    stub(fakeModel, "getData").returns(fakeModelData);
    stub(fakeModel, "setProperty");
    stub(fakeSource, "data").withArgs("childrenLoaded").returns(false);
    stub(fakeSource, "getProperty").withArgs("key").returns(fakeKey);
    stub(fakeSource, "setCollapsed");
    stub(ServiceUtils, "getDataSource").withArgs("restService").returns({
      uri: "rest/v1",
    });
    stub(ServiceUtils, "getUrl");
    stub(AsyncUtils, "finally");
    stub(RestClient, "get").returns(fakePromise);

    // Act
    controller.updateImpactAnalysis(fakeSource, fakeId, fakeTrackingIdType);

    // Assert
    fakePromise.then(function (data) {
      assert.deepEqual(fakeModel.setProperty.args[0], ["/isLoaded", false], "Impact analysis is loading");
      assert.deepEqual(fakeModel.setProperty.args[1], ["/nodes", data.nodes], "Nodes have been updated");
      assert.deepEqual(fakeModel.setProperty.args[2], ["/lines", []], "Lines have been updated");
      assert.deepEqual(fakeModel.setProperty.args[3], ["/isLoaded", true], "Impact analysis has been loaded");
      assert.ok(fakeSource.data.calledWith("childrenLoaded", true), "'childrenLoaded' has been set to true");
      assert.ok(fakeSource.setCollapsed.calledWith(false), "Node has been expanded");
      done();
    });
  });
});
