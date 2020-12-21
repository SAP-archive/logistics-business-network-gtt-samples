sap.ui.define(["com/sap/gtt/app/sample/pof/util/ServiceUtils"], function (ServiceUtils) {
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
    var component = {};
    var dataSource = {
      uri: "some/path",
      type: "JSON",
    };
    stub(component, "getManifestEntry").returns(dataSource);

    // Act
    var options = {
      component: component,
    };
    ServiceUtils.init(options);
    var result = ServiceUtils.getDataSource("someService");

    // Assert
    assert.strictEqual(result, dataSource, "data source is correct");
  });

  QUnit.test("getUrl", function (assert) {
    // Arrange
    var component = {};
    var manifestObject = {};
    stub(manifestObject, "getComponentName").returns("com.sap.gtt.app.sample.pof");
    stub(component, "getManifestObject").returns(manifestObject);

    // Act
    var options = {
      component: component,
    };
    ServiceUtils.init(options);
    var url = ServiceUtils.getUrl("some/path");

    // Assert
    assert.ok(url.endsWith("some/path"), "url is correct");
  });
});
