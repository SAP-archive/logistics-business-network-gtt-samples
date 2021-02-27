sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/SalesOrderList.controller",
], function (SalesOrderList) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.SalesOrderList", {
    beforeEach: function () {
      this.controller = new SalesOrderList();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("onAfterVariantLoad", function (assert) {
    var controller = this.controller;

    // Arrange
    controller.routeName = "salesOrderList";
    var fakeModel = {};
    var fakeSmartFilterBar = {};
    stub(controller, "getModel").withArgs(controller.routeName).returns(fakeModel);
    stub(fakeModel, "setProperty");
    stub(controller, "byId").withArgs("smartFilterBar").returns(fakeSmartFilterBar);
    stub(fakeSmartFilterBar, "getFilterData").returns({
      _CUSTOM: {
        test: {
          type: "Edm.DateTimeoffset",
          low: new Date(),
          high: new Date(),
        },
      },
    });
    stub(fakeSmartFilterBar, "search");

    // Act
    controller.onAfterVariantLoad();

    // Assert
    assert.ok(controller.getModel(controller.routeName).setProperty.calledOnce, "The customFilters property is set.");
  });

  QUnit.test("onBeforeVariantSave", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeEvent = {};
    stub(fakeEvent, "getParameter").withArgs("context").returns("STANDARD");
    stub(controller, "updateCustomFilter");

    // Act
    controller.onBeforeVariantSave(fakeEvent);

    // Assert
    assert.ok(controller.updateCustomFilter.calledOnce, "The updateCustomFilter function is called.");
  });

  QUnit.test("onBeforeVariantFetch", function (assert) {
    var controller = this.controller;

    // Arrange
    stub(controller, "updateCustomFilter");

    // Act
    controller.onBeforeVariantFetch();

    // Assert
    assert.ok(controller.updateCustomFilter.calledOnce, "The updateCustomFilter function is called.");
  });

  QUnit.test("updateCustomFilter", function (assert) {
    var controller = this.controller;

    // Arrange
    controller.routeName = "salesOrderList";
    var fakeModel = {};
    var fakeSmartFilterBar = {};
    stub(controller, "getModel").withArgs(controller.routeName).returns(fakeModel);
    stub(fakeModel, "getProperty").withArgs("/customFilters").returns("CustomerFilters");
    stub(controller, "byId").withArgs("smartFilterBar").returns(fakeSmartFilterBar);
    stub(fakeSmartFilterBar, "setFilterData");

    // Act
    controller.updateCustomFilter();

    // Assert
    assert.ok(controller.byId("smartFilterBar").setFilterData.calledOnce, "The setFilterData function is called.");
  });
});
