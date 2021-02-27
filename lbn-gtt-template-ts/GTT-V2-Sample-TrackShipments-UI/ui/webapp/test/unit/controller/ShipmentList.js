sap.ui.define([
  "com/sap/gtt/app/sample/sst/controller/ShipmentList.controller",
  "sap/ui/model/Filter",
  "sap/ui/model/FilterOperator",
], function (ShipmentList, Filter, FilterOperator) {
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

  QUnit.module("com.sap.gtt.app.sst.controller.ShipmentList", {
    beforeEach: function () {
      this.controller = new ShipmentList();

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
    var fakeModel = {};
    var fakeSmartFilterBar = {};
    stub(controller, "getModel").withArgs("view").returns(fakeModel);
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
    assert.ok(controller.getModel("view").setProperty.calledOnce, "The customFilters property is set.");
  });

  QUnit.test("refineFilters - location filter", function (assert) {
    var controller = this.controller;

    // Arrange
    var fakeFilterData = {
      departureLocationId: {},
      arrivalLocationId: {},
    };

    var fakeLocationFilter = new Filter("departureLocationId", FilterOperator.EQ, "xri://sap.com/id:LBN#10010001006:QM7CLNT910:Location:Customer:0000010105");
    var fakeFilters = [{
      aFilters: [fakeLocationFilter],
      bAnd: false,
    }];

    // Act
    controller.refineFilters(fakeFilterData, fakeFilters);

    // Assert
    assert.equal(fakeFilters[0].aFilters[0].aFilters[0].oValue1, "0000010105", "The departureLocationId is right");
    assert.equal(fakeFilters[0].aFilters[0].aFilters[1].sPath, "departureLocationType_code", "The departureLocationType_code will be filtered");
    assert.equal(fakeFilters[0].aFilters[0].aFilters[1].oValue1, "Customer", "The departure location type is right");
    assert.equal(fakeFilters[0].aFilters[0].aFilters[2].sPath, "logicalSystem", "The logicalSystem will be filtered");
    assert.equal(fakeFilters[0].aFilters[0].aFilters[2].oValue1, "QM7CLNT910", "The logicalSystem is right");
  });
});
