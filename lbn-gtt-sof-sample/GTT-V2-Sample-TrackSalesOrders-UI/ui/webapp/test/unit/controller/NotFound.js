sap.ui.define([
  "com/sap/gtt/app/sample/sof/controller/NotFound.controller",
], function (NotFound) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.NotFound", {
    beforeEach: function () {
      this.controller = new NotFound();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("onLinkPressed", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeRouter = {};
    stub(fakeRouter, "navTo");
    stub(controller, "getRouter").returns(fakeRouter);

    // Act
    controller.onLinkPressed();

    // Assert
    assert.ok(fakeRouter.navTo.calledOnce, "Will go to list page");
  });
});
