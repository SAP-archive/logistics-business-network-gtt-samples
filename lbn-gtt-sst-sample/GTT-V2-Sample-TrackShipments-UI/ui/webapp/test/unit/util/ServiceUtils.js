sap.ui.define(["com/sap/gtt/app/sample/sst/util/ServiceUtils"], function (ServiceUtils) {
  "use strict";

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

  QUnit.module("util/ServiceUtils", {
    beforeEach: function () {},
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("getDataSource", function (assert) {
    // Arrange
    var restService = {
      "uri": "trackshipments/sap/logistics/gtt/sample/sst/rest/v1",
      "type": "JSON",
      "settings": {
        "localUri": "localService/mockdata/",
        "maxAge": 30,
      },
    };

    // Act
    ServiceUtils.init();
    var result = ServiceUtils.getDataSource("restService");

    // Assert
    assert.deepEqual(result, restService, "data source is correct");
  });

  QUnit.test("getUrl", function (assert) {
    // Arrange
    var component = {};
    var manifestObject = {};
    stub(manifestObject, "getComponentName").returns("com.sap.gtt.app.sample.sst");
    stub(component, "getManifestObject").returns(manifestObject);

    // Act
    ServiceUtils.init();
    var url = ServiceUtils.getUrl("some/path");

    // Assert
    assert.ok(url.endsWith("some/path"), "url is correct");
  });
});
