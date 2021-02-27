sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/SalesOrderItem.controller",
], function (SalesOrderItem) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.SalesOrderItem", {
    beforeEach: function () {
      this.controller = new SalesOrderItem();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("onBindingChange - no binding context", function (assert) {
    var controller = this.controller;

    // Arrange
    stub(controller, "getModel");
    stub(controller, "setViewFree");

    var fakeView = controller.getView();
    stub(fakeView, "getBindingContext").returns(null);
    var fakeRouter = {};
    var fakeTargets = {};
    stub(fakeTargets, "display");
    stub(fakeRouter, "getTargets").returns(fakeTargets);
    stub(controller, "getRouter").returns(fakeRouter);

    // Act
    controller.onBindingChange();

    // Assert
    assert.ok(fakeTargets.display.calledWith("notFound"), "Sales order item is not found");
  });
});
