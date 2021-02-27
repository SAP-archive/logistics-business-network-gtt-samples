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

  QUnit.module("com.sap.gtt.app.pof.controller.PurchaseOrderList", {
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
    assert.equal(oFakeFilter.aFilters[0].sPath, "purchaseOrderItemTPs/materialId", "The filter path is changed.");
  });

  QUnit.test("_setFilterPath - set delivery filters", function (assert) {
    // Arrange
    var sResultPath = "purchaseOrderItemTPs/inboundDeliveryItems/inboundDelivery/inboundDeliveryNo";
    var oFakeFilter = {
      "aFilters": [
        {
          "sPath": "inboundDeliveryItems/inboundDelivery/inboundDeliveryNo",
        },
      ],
    };

    // Act
    var oResult = this.oListController._setFilterPath(oFakeFilter);

    // Assert
    assert.ok(!oResult, "The startup parameters are got");
    assert.equal(oFakeFilter.aFilters[0].sPath, sResultPath);
  });

  QUnit.test("_setFilterPath - set shipment filters", function (assert) {
    // Arrange
    var sResultPath = "purchaseOrderItemTPs/inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo";
    var oFakeFilter = {
      "aFilters": [
        {
          "sPath": "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo",
        },
      ],
    };

    // Act
    var oResult = this.oListController._setFilterPath(oFakeFilter);

    // Assert
    assert.ok(!oResult, "The startup parameters are got");
    assert.equal(oFakeFilter.aFilters[0].sPath, sResultPath);
  });

  QUnit.test("_getResultPOIFilters - the path to delivery filter should be changed", function (assert) {
    // Arrange
    var sExpResultPath = "inboundDeliveryItems/inboundDelivery/inboundDeliveryNo";
    var oFakeFilter = {
      "inboundDeliveryItems.inboundDelivery.inboundDeliveryNo": {
        "inputValue": "5",
        "filter": {
          "sPath": "purchaseOrderItemTPs/inboundDeliveryItems/inboundDelivery/inboundDeliveryNo",
        },
      },
    };
    var oFakeModel = {};
    var oComponent = {};

    stub(oComponent, "routeName").returns("routeName");
    stub(this.oListController, "getModel").returns(oFakeModel);
    stub(oFakeModel, "getProperty").returns(oFakeFilter);

    // Act
    var oResultFilter = this.oListController._getResultPOIFilters([]);

    // Assert
    assert.ok(oResultFilter, "The Result Filter exists");
    assert.equal(oResultFilter.aFilters.length, 1, "Only one filter is implemented.");
    assert.equal(oResultFilter.aFilters[0].sPath, sExpResultPath);
  });

  QUnit.test("_getResultPOIFilters - the path to shipment filter should be changed", function (assert) {
    // Arrange
    var sExpResultPath = "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo";
    var oFakeFilter = {
      "inboundDeliveryItems.inboundDelivery.inboundDeliveryNo": {
        "inputValue": "5",
        "filter": {
          "sPath": "purchaseOrderItemTPs/inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo",
        },
      },
    };
    var oFakeModel = {};
    var oComponent = {};

    stub(oComponent, "routeName").returns("routeName");
    stub(this.oListController, "getModel").returns(oFakeModel);
    stub(oFakeModel, "getProperty").returns(oFakeFilter);

    // Act
    var oResultFilter = this.oListController._getResultPOIFilters([]);

    // Assert
    assert.ok(oResultFilter, "The Result Filter exists");
    assert.equal(oResultFilter.aFilters.length, 1, "Only one filter is implemented.");
    assert.equal(oResultFilter.aFilters[0].sPath, sExpResultPath);
  });

  QUnit.test("_getResultPOIFilters - the standard and custom filter for POItems should works together in 'and' relation", function (assert) {
    // Arrange
    var sExpResultPath = "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo";
    var oFakeFilter = {
      "inboundDeliveryItems.inboundDelivery.inboundDeliveryNo": {
        "inputValue": "5",
        "filter": {
          "sPath": "purchaseOrderItemTPs/inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo",
        },
      },
    };
    var aFakeStandardFilter = [
      {
        "aFilters": [
          {
            "sPath": "supplierId",
            "sOperator": "EQ",
            "oValue1": "56",
            "_bMultiFilter": false,
          },
        ],
        "bAnd": false,
        "_bMultiFilter": true,
      },
    ];
    var oFakeModel = {};
    var oComponent = {};

    stub(oComponent, "routeName").returns("routeName");
    stub(this.oListController, "getModel").returns(oFakeModel);
    stub(oFakeModel, "getProperty").returns(oFakeFilter);

    // Act
    var oResultFilter = this.oListController._getResultPOIFilters(aFakeStandardFilter);

    // Assert
    assert.ok(oResultFilter, "The Result Filter exists");
    assert.equal(oResultFilter.aFilters.length, 2, "One standard and one custom filter are implemented.");
    assert.ok(oResultFilter.bAnd, "'And' relation between filteers");
    assert.equal(oResultFilter.aFilters[1].aFilters[0].sPath, sExpResultPath);
  });

  QUnit.test("_addCustomFilters - add custom filters", function (assert) {
    // Arrange
    var oFakeFilter = {
      "inboundDeliveryItems.inboundDelivery.shipmentTPs.shipment.shipmentNo": {
        "inputValue": "4",
        "filter": {
          "sPath": "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo",
          "sOperator": "Contains",
          "oValue1": "4",
          "_bMultiFilter": false,
        },
      },
    };
    var oFakeModel = {};
    var oComponent = {};

    stub(oComponent, "routeName").returns("routeName");
    stub(this.oListController, "getModel").returns(oFakeModel);
    stub(oFakeModel, "getProperty").returns(oFakeFilter);

    // Act
    var oResultFilter = this.oListController._addCustomFilters([]);
    var sExpectedPath = "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo";
    // Assert
    assert.ok(!!oResultFilter && !!oResultFilter.aFilters.length, "The result filter exists");
    assert.equal(oResultFilter.aFilters[0].sPath, sExpectedPath);
  });
  QUnit.test("_addCustomFilters - add standard and custom filters", function (assert) {
    // Arrange
    var aStandardFilters = [
      {
        "aFilters": [
          {
            "sPath": "purchaseOrderNo",
            "sOperator": "EQ",
            "oValue1": "344",
            "_bMultiFilter": false,
          },
        ],
        "bAnd": false,
        "_bMultiFilter": true,
      },
    ];
    var oFakeFilter = {
      "inboundDeliveryItems.inboundDelivery.shipmentTPs.shipment.shipmentNo": {
        "inputValue": "4",
        "filter": {
          "sPath": "inboundDeliveryItems/inboundDelivery/shipmentTPs/shipment/shipmentNo",
          "sOperator": "Contains",
          "oValue1": "4",
          "_bMultiFilter": false,
        },
      },
    };
    var oFakeModel = {};
    var oComponent = {};

    stub(oComponent, "routeName").returns("routeName");
    stub(this.oListController, "getModel").returns(oFakeModel);
    stub(oFakeModel, "getProperty").returns(oFakeFilter);

    // Act
    var oResultFilter = this.oListController._addCustomFilters(aStandardFilters);

    // Assert
    assert.ok(!!oResultFilter && !!oResultFilter.aFilters.length, "The result filter exists");
    assert.equal(oResultFilter.aFilters.length, 2);
    assert.ok(!!oResultFilter.bAnd, "Standard and custom filters have 'and' relation.");
  });

});
