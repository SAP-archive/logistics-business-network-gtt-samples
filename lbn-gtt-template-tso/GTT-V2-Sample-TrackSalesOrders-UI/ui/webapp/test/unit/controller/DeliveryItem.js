sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/DeliveryItem.controller",
], function (DeliveryItem) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.DeliveryItem", {
    beforeEach: function () {
      this.controller = new DeliveryItem();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("onRefreshPressed", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeBinding = {};
    stub(fakeBinding, "refresh");
    stub(controller.getView(), "getElementBinding").returns(fakeBinding);

    // Act
    controller.onRefreshPressed();

    // Assert
    assert.ok(fakeBinding.refresh.calledWith(true), "The view will be refreshed");
  });

  QUnit.test("updateLocationQuickViewData", function (assert) {
    var controller = this.controller;

    // Arrange
    controller.locationQuickView = {};
    var fakeModel1 = {};
    var fakeModel2 = {};
    var fakeDestination = {};
    stub(fakeModel1, "setProperty");
    stub(fakeModel2, "getProperty").withArgs("/destination").returns(fakeDestination);
    stub(controller.locationQuickView, "getModel").returns(fakeModel1);
    stub(controller, "getModel").returns(fakeModel2);

    // Act
    controller.updateLocationQuickViewData();

    // Assert
    assert.ok(fakeModel1.setProperty.calledWith("/", fakeDestination), "The destination is updated");
  });
});
