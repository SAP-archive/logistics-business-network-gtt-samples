sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/m/MessageBox",
  "sap/ui/core/Fragment",
], function (BaseController, MessageBox, Fragment) {
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

  QUnit.module("com.sap.gtt.app.pof.controller.BaseController", {
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

    controller.getOwnerComponent.restore();
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

    fakeResourceBundle.getText.restore();
    fakeModel.getResourceBundle.restore();
    fakeComponent.getModel.restore();
    controller.getOwnerComponent.restore();
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

    fakeComponent.getManifestEntry.restore();
    controller.getOwnerComponent.restore();
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

    controller.handleError.restore();
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

    MessageBox.error.restore();
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

    MessageBox.error.restore();
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

    MessageBox.error.restore();
  });
});
