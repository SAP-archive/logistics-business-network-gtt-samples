sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/DocumentFlow.controller",
], function (DocumentFlow) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.DocumentFlow", {
    beforeEach: function () {
      this.controller = new DocumentFlow();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("navToDetailPage- nav to sales order item page", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeView = controller.getView();
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty").withArgs("id").returns("fakeId");
    stub(fakeView, "getBindingContext").returns(fakeBindingContext);

    var fakeEvent = {};
    var fakeSource = {};
    var fakeBindingContext2 = {};
    stub(fakeBindingContext2, "getProperty")
      .withArgs("id").returns("fakeId2")
      .withArgs("group").returns(2);
    stub(fakeSource, "getBindingContext").returns(fakeBindingContext2);
    stub(fakeEvent, "getSource").returns(fakeSource);

    var fakeRouter = {};
    stub(fakeRouter, "navTo");
    stub(controller, "getRouter").returns(fakeRouter);

    // Act
    controller.navToDetailPage(fakeEvent);

    // Assert
    assert.ok(fakeRouter.navTo.calledWith("salesOrderItem", {
      id: "fakeId2",
      params: {
        salesOrderId: "fakeId",
      },
    }), "The navigation is triggered");
  });

  QUnit.test("navToDetailPage- nav to delivery item page", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeView = controller.getView();
    var fakeBindingContext = {};
    stub(fakeBindingContext, "getProperty").withArgs("id").returns("fakeId");
    stub(fakeView, "getBindingContext").returns(fakeBindingContext);

    var fakeEvent = {};
    var fakeSource = {};
    var fakeBindingContext2 = {};
    stub(fakeBindingContext2, "getProperty")
      .withArgs("id").returns("fakeId2")
      .withArgs("group").returns(3);
    stub(fakeSource, "getBindingContext").returns(fakeBindingContext2);
    stub(fakeEvent, "getSource").returns(fakeSource);

    var fakeRouter = {};
    stub(fakeRouter, "navTo");
    stub(controller, "getRouter").returns(fakeRouter);

    // Act
    controller.navToDetailPage(fakeEvent);

    // Assert
    assert.ok(fakeRouter.navTo.calledWith("deliveryItem", {
      id: "fakeId2",
      params: {
        salesOrderId: "fakeId",
      },
    }), "The navigation is triggered");
  });
});
