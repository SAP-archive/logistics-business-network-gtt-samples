sap.ui.define([
  "com/sap/gtt/app/sample/sst/controller/Shipment.controller",
  "sap/ui/model/Filter",
  "sap/ui/model/FilterOperator",
], function (Shipment, Filter, FilterOperator) {
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

  QUnit.module("com.sap.gtt.app.sst.controller.Shipment", {
    beforeEach: function () {
      this.controller = new Shipment();

      var fakeView = {};
      stub(this.controller, "getView").returns(fakeView);
    },
    afterEach: function () {
      sandbox.restore();
      this.controller.destroy();
    },
  });

  QUnit.test("addVpRowInRefDocumentsTable - no track id", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeSource = {};
    stub(fakeSource, "data");
    var fakeRow = {};
    stub(fakeRow, "destroy");
    stub(controller, "byId").withArgs("vpRow").returns(fakeRow);

    // Act
    controller.addVpRowInRefDocumentsTable(false, fakeSource, undefined);

    // Assert
    assert.ok(fakeSource.data.calledWith("isVpRowAdded", false), "The custom data is updated");
    assert.ok(fakeRow.destroy.calledOnce, "VP row is removed");
  });

  QUnit.test("addVpRowInRefDocumentsTable - update track id", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeSource = {};
    stub(fakeSource, "data").withArgs("isVpRowAdded").returns(true);
    var fakeRow = {};
    var fakeText = {};
    stub(fakeText, "setText");
    stub(controller, "byId")
      .withArgs("vpRow").returns(fakeRow)
      .withArgs("trackIdText").returns(fakeText);

    // Act
    controller.addVpRowInRefDocumentsTable(false, fakeSource, "ID1");

    // Assert
    assert.ok(fakeText.setText.calledWith("ID1"), "The track Id is updated");
  });
});
