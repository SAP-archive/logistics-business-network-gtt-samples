sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/BaseDetailController",
], function (BaseDetailController) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.BaseDetailController", {
    beforeEach: function () {
      this.controller = new BaseDetailController();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("getExpandList", function (assert) {
    var controller = this.controller;

    // Act
    var list = controller.getExpandList();

    // Assert
    assert.ok(list.length === 0, "There is no expand list");
  });

  QUnit.test("refreshSubSection", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeView = {};
    var fakeController = {};
    stub(fakeController, "refresh");
    stub(fakeView, "getController").returns(fakeController);
    stub(controller, "byId").returns(fakeView);

    // Act
    controller.refreshSubSection("subSectionId");

    // Assert
    assert.ok(fakeController.refresh.calledOnce, "Subsection will refresh");
  });

  QUnit.test("navToSalesOrderList", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeRouter = {};
    stub(fakeRouter, "navTo");
    stub(controller, "getRouter").returns(fakeRouter);

    // Act
    controller.navToSalesOrderList();

    // Assert
    assert.ok(fakeRouter.navTo.calledWith("salesOrderList"), "Sales order list will show");
  });

  QUnit.test("navToSalesOrder", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeRouter = {};
    stub(fakeRouter, "navTo");
    stub(controller, "getRouter").returns(fakeRouter);

    var fakeEvent = {};
    var fakeSource = {};
    stub(fakeSource, "data").withArgs("salesOrderId").returns("id");
    stub(fakeEvent, "getSource").returns(fakeSource);

    // Act
    controller.navToSalesOrder(fakeEvent);

    // Assert
    assert.ok(fakeRouter.navTo.calledWith("salesOrder", {id: "id"}), "Sales order will show");
  });
});
