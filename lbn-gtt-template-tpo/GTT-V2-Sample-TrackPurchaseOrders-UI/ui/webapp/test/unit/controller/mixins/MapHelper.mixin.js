sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/mixins/MapHelper.mixin",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
  "sap/ui/vbm/SemanticType",
], function (MapHelper, ServiceUtils, RestClient, SemanticType) {
  "use strict";

  // mockserver.init();

  var sandbox = sinon.sandbox.create();

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

  QUnit.module("com.sap.gtt.app.sample.pof.controller.mixins.MapHelper.mixin", {
    beforeEach: function () {
      var oFakeModel = {};
      var oComponent = {};

      stub(oComponent, "getModel").returns(oFakeModel);

      this.oMapHelper = MapHelper;
      this.oMapHelper.oMapModel = oFakeModel;
    },
    afterEach: function () {
      sandbox.restore();
      this.oMapHelper = null;
    },
  });

  QUnit.test("zoomRouteAndStopsToAreas: no zoom route", function (assert) {
    // Arrange
    var oFakeArray = [];
    stub(this.oMapHelper.oMapModel, "getProperty").returns(oFakeArray);

    // Assert
    var oResult = this.oMapHelper.zoomRouteAndStopsToAreas();
    assert.ok(!oResult, "Return undefined");
  });

  QUnit.test("zoomRouteAndStopsToAreas: zoom route", function (assert) {
    // Arrange
    var oFakeArray = [{
      "plannedSpots": [
        {
          "longitude": 78.64741,
          "latitude": 28.53406,
        },
        {
          "longitude": -0.2416796,
          "latitude": 51.5287718,
        },
        {
          "longitude": 77.797708,
          "latitude": 12.7379224,
        },
      ],
      "actualSpots": [
        {
          "longitude": 16.9698985,
          "latitude": 50.0968903,
        },
        {
          "longitude": 26.9698985,
          "latitude": 50.0968903,
        },
        {
          "longitude": 25.320548,
          "latitude": 57.478041,
        },
      ],
    }];
    var oFakePositionsArray = [
      [78.64741, 28.53406],
      [97.6686366, 18.6942646],
      [97.6686366, 18.6942646],
    ];
    var oFakeControl = {};

    stub(this.oMapHelper.oMapModel, "getProperty").returns(oFakeArray);
    stub(this.oMapHelper, "getPositionArray").returns(oFakePositionsArray);
    stub(this.oMapHelper, "byId").returns(oFakeControl);
    stub(oFakeControl, "zoomToAreas").returns("success");

    // Assert
    var oResult = this.oMapHelper.zoomRouteAndStopsToAreas();
    assert.ok(!oResult, "Return undefined");
  });

  QUnit.test("getStopIcon: journey depart location icon", function (assert) {
    // Arrange
    var sLocationType = "ShippingPoint";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "sap-icon://journey-depart", "Return journey-depart icon");
  });

  QUnit.test("getStopIcon: visits location icon", function (assert) {
    // Arrange
    var sLocationType = "Customer";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "sap-icon://visits", "Return visits icon");
  });

  QUnit.test("getStopIcon: icon-warehouse location icon", function (assert) {
    // Arrange
    var sLocationType = "Plant";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "sap-icon://BusinessSuiteInAppSymbols/icon-warehouse", "Return icon-warehouse icon");
  });

  QUnit.test("getStopIcon: factory location icon", function (assert) {
    // Arrange
    var sLocationType = "Supplier";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "sap-icon://factory", "Return factory icon");
  });

  QUnit.test("getStopIcon: functional-location location icon", function (assert) {
    // Arrange
    var sLocationType = "LogisticLocation";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "sap-icon://functional-location", "Return functional-location icon");
  });

  QUnit.test("getStopIcon: default location icon", function (assert) {
    // Arrange
    var sLocationType = "";
    // Assert
    var oResult = this.oMapHelper.getStopIcon(sLocationType);
    assert.ok(oResult === "", "Return default icon");
  });

  QUnit.test("getSpotType: type for delayed spot", function (assert) {
    // Arrange
    var sEventStatusCode = "DELAYED";
    // Assert
    var oResult = this.oMapHelper.getSpotType(sEventStatusCode);
    assert.ok(oResult === SemanticType.Error, "Return Error semantic type");
  });

  QUnit.test("getSpotType: type for overdue spot", function (assert) {
    // Arrange
    var sEventStatusCode = "OVERDUE";
    // Assert
    var oResult = this.oMapHelper.getSpotType(sEventStatusCode);
    assert.ok(oResult === SemanticType.Warning, "Return Warning semantic type");
  });

  QUnit.test("getSpotType: default type", function (assert) {
    // Arrange
    var sEventStatusCode = "";
    // Assert
    var oResult = this.oMapHelper.getSpotType(sEventStatusCode);
    assert.ok(oResult === SemanticType.Default, "Return Default semantic type");
  });

  QUnit.test("getHereMapKey: return request with key", function (assert) {
    // Arrange
    var oFakeDataSource = {
      uri: "fakeURI",
    };
    var sFakeUrl = "url";
    var oFakeRequest = new Promise(function (resolve, reject) {
      resolve({key: 1});
    });
    stub(ServiceUtils, "getUrl").returns(sFakeUrl);
    stub(ServiceUtils, "getDataSource").returns(oFakeDataSource);
    stub(RestClient, "get").returns(oFakeRequest);
    stub(this.oMapHelper, "keys").returns({});
    // Assert
    var oResult = this.oMapHelper.getHereMapKey();
    assert.ok(oResult instanceof Object, "Return request object");
  });
});
