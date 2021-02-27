sap.ui.define([
  "com/sap/gtt/app/sample/sst/controller/BaseController",
  "sap/m/MessageBox",
], function (
  BaseController,
  MessageBox) {
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

  QUnit.module("com.sap.gtt.app.sst.controller.BaseController", {
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

  QUnit.test("handleLocationQuickViewAfterOpen", function (assert) {
    var controller = this.controller;

    // Arrange
    controller.selectedLocation = {};
    controller.locationQuickView = {};
    stub(controller.selectedLocation, "data")
      .withArgs("modelName").returns("model")
      .withArgs("propertyName").returns("prop");
    stub(controller.selectedLocation, "getModel");
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty").withArgs("prop").returns({});
    stub(controller.selectedLocation, "getBindingContext")
      .withArgs("model").returns(fakeBindingContext);

    var fakeModel = {};
    stub(fakeModel, "setProperty");
    stub(controller.locationQuickView, "getModel").returns(fakeModel);

    // Act
    controller.handleLocationQuickViewAfterOpen();

    // Assert
    assert.ok(fakeModel.setProperty.calledWith("/", {}), "The location is updated");
  });
});
