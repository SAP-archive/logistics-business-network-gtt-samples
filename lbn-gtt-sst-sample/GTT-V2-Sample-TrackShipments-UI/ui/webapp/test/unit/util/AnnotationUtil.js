sap.ui.define(["com/sap/gtt/app/sample/sst/util/AnnotationUtil"], function (AnnotationUtil) {
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

  QUnit.test("getPropertyLabel - no property label annotation", function (assert) {
    // Arrange
    var fakeMetaModel = {};
    stub(fakeMetaModel, "getODataEntityContainer").returns({
      namespace: "com.sap.gtt.sample",
    });
    var fakeAnnotations = {
      propertyAnnotations: {
        "com.sap.gtt.sample.shipment": {},
      },
    };
    AnnotationUtil.init({
      metaModel: fakeMetaModel,
      annotations: fakeAnnotations,
    });

    // Act
    var label = AnnotationUtil.getPropertyLabel("delay", "shipment");

    // Assert
    assert.ok(label === "delay", "The label is the property name");
  });
});
