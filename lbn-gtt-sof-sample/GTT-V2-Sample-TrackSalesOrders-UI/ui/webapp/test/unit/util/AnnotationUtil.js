sap.ui.define(["com/sap/gtt/app/sample/sof/util/AnnotationUtil"], function (AnnotationUtil) {
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

  QUnit.module("util/AnnotationUtil", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("getPropertyLabel - no such property", function (assert) {
    // Arrange
    stub(AnnotationUtil, "getEntityTypeByEntitySet");
    stub(AnnotationUtil, "getProperty").returns(null);

    // Act
    var label = AnnotationUtil.getPropertyLabel("prop", "entitySet");

    // Assert
    assert.ok(label === "prop", "The label is the property name");
  });

  QUnit.test("getPropertyLabel - no property label annotation", function (assert) {
    // Arrange
    stub(AnnotationUtil, "getEntityTypeByEntitySet");
    stub(AnnotationUtil, "getProperty").returns({
      name: "propName",
    });

    // Act
    var label = AnnotationUtil.getPropertyLabel("prop", "entitySet");

    // Assert
    assert.ok(label === "propName", "The label is the property name");
  });
});
