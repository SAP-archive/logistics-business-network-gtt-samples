sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/BaseController",
  "com/sap/gtt/app/sample/sof/model/type/ISODate",
  "com/sap/gtt/app/sample/sof/model/type/ISODateTime",
  "sap/m/MessageBox",
  "sap/ui/core/Fragment",
], function (BaseController, ISODate, ISODateTime, MessageBox, Fragment) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.BaseController", {
    beforeEach: function () {
      this.controller = new BaseController();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("type - ISODate", function (assert) {
    // Act
    var isoDate = new ISODate();
    var formattedDate = isoDate.formatValue("2012-01-02", "string");

    // Assert
    assert.ok(formattedDate, "ISO date is right");
  });

  QUnit.test("type - ISODateTime", function (assert) {
    // Act
    var isoDateTime = new ISODateTime();
    var formattedDateTime = isoDateTime.formatValue("2012-01-02T12:30:30Z", "string");

    // Assert
    assert.ok(formattedDateTime, "ISO datetime is right");
  });

  QUnit.test("getStartupParameters", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeComponent = {};
    stub(fakeComponent, "getComponentData").returns({
      startupParameters: {id: "abc"},
    });
    stub(controller, "getOwnerComponent").returns(fakeComponent);

    // Act
    var startupParameters = controller.getStartupParameters();

    // Assert
    assert.ok(startupParameters.id === "abc", "The startup parameters are got");
  });

  QUnit.test("getStartupParameters - no component data", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeComponent = {};
    stub(fakeComponent, "getComponentData").returns(null);
    stub(controller, "getOwnerComponent").returns(fakeComponent);

    // Act
    var startupParameters = controller.getStartupParameters();

    // Assert
    assert.deepEqual(startupParameters, {}, "The startup parameters are got");
  });

  QUnit.test("getResourceBundle, getText", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeComponent = {};
    var fakeResourceBundle = {};
    stub(fakeResourceBundle, "getText").withArgs("key").returns("value");
    var fakeModel = {};
    stub(fakeModel, "getResourceBundle").returns(fakeResourceBundle);
    stub(fakeComponent, "getModel").withArgs("i18n").returns(fakeModel);
    stub(controller, "getOwnerComponent").returns(fakeComponent);

    // Act
    var rb = controller.getResourceBundle();
    var text = controller.getText("key");

    // Assert
    assert.equal(rb, fakeResourceBundle, "The resource bundle is got");
    assert.equal(text, "value", "The text is got");
  });

  QUnit.test("getComponentConfig", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeComponent = {};
    var fakeConfig = {};
    stub(fakeComponent, "getManifestEntry").withArgs("/sap.ui5/config").returns(fakeConfig);
    stub(controller, "getOwnerComponent").returns(fakeComponent);

    // Act
    var config = controller.getComponentConfig();

    // Assert
    assert.equal(config, fakeConfig, "The config is got");
  });

  QUnit.test("handleServerError", function (assert) {
    var controller = this.controller;

    // Arrange
    stub(controller, "handleError");
    var fakeError = {
      response: {
        date: {},
      },
    };

    // Act
    controller.handleServerError(fakeError);

    // Assert
    assert.ok(controller.handleError.calledOnce, "The error will be handled");
  });

  QUnit.test("handleError", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeError = {
      error: {
        message: "failed",
      },
    };
    stub(MessageBox, "error");

    // Act
    controller.handleError(fakeError);

    // Assert
    assert.ok(MessageBox.error.calledOnce, "The error is shown");
  });

  QUnit.test("handleError - plain object", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeError = "error message";
    stub(MessageBox, "error");

    // Act
    controller.handleError(fakeError);

    // Assert
    assert.ok(MessageBox.error.calledWith(fakeError), "The error is shown");
  });

  QUnit.test("handleError - details", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeError = {
      error: {
        details: [{
          message: "detail message",
        }],
        message: "detail messages",
      },
    };
    stub(MessageBox, "error");

    // Act
    controller.handleError(fakeError);

    // Assert
    assert.ok(MessageBox.error.calledWith(fakeError.error.message), "The error is shown");
  });

  QUnit.test("openLocationQuickView - no location quickview", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeEvent = {};
    stub(fakeEvent, "getSource");
    var fakeLocationQuickView = {};
    stub(fakeLocationQuickView, "openBy");
    stub(fakeLocationQuickView, "setModel");
    var fakeView = controller.getView();
    stub(fakeView, "addDependent");
    stub(controller, "createId");

    var fakePromise = {};
    stub(fakePromise, "then");
    stub(Fragment, "load").returns(fakePromise);

    // Act
    controller.openLocationQuickView(fakeEvent);

    // Assert
    var resolveFn = fakePromise.then.firstCall.args[0];
    resolveFn(fakeLocationQuickView);
    assert.ok(controller.locationQuickView !== undefined, "The location quickview is loaded");
    assert.ok(controller.locationQuickView.openBy.calledOnce, "The location quickview will open");
  });

  QUnit.test("openLocationQuickView - has location quickview", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeEvent = {};
    stub(fakeEvent, "getSource");
    var fakeLocationQuickView = {};
    stub(fakeLocationQuickView, "openBy");
    controller.locationQuickView = fakeLocationQuickView;

    // Act
    controller.openLocationQuickView(fakeEvent);

    // Assert
    assert.ok(controller.locationQuickView.openBy.calledOnce, "The location quickview will open");
  });

  QUnit.test("handleLocationQuickViewAfterOpen", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeSelectedLocation = {};
    stub(fakeSelectedLocation, "data")
      .withArgs("modelName").returns("map")
      .withArgs("propertyName").returns("location");
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty")
      .withArgs("location").returns({});
    stub(fakeSelectedLocation, "getBindingContext").withArgs("map").returns(fakeBindingContext);

    var fakeLocationQuickView = {};
    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(fakeLocationQuickView, "getModel").returns(fakeModel);
    controller.selectedLocation = fakeSelectedLocation;
    controller.locationQuickView = fakeLocationQuickView;

    // Act
    controller.handleLocationQuickViewAfterOpen();

    // Assert
    assert.ok(fakeModel.setProperty.calledWith("/", {}), "The location data is updated");
  });
});
