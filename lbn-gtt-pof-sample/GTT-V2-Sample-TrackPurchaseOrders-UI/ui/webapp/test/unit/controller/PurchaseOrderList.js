sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/list/PurchaseOrderList.controller",
  "com/sap/gtt/app/sample/pof/util/Constants",
], function (PurchaseOrderList, Constants) {
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

  QUnit.module("com.sap.gtt.app.sof.controller.PurchaseOrderList", {
    beforeEach: function () {
      this.oListController = new PurchaseOrderList();

    },
    afterEach: function () {
      sandbox.restore();
      this.oListController.destroy();
    },
  });


  QUnit.test("_setFilterPath - set filters", function (assert) {
    // Arrange
    var oFakeFilter = {
      "aFilters": [
        {
          "sPath": "materialId",
          "sOperator": "Contains",
          "oValue1": "123",
          "_bMultiFilter": false,
        },
      ],
      "bAnd": false,
      "_bMultiFilter": true,
    };

    // Act
    var oResult = this.oListController._setFilterPath(oFakeFilter);

    // Assert
    assert.ok(!oResult, "The startup parameters are got");
    assert.equal(oFakeFilter.aFilters[0].sPath, "purchaseOrderItemTPs/purchaseOrderItem/materialId", "The filter path is changed.");
  });
});
