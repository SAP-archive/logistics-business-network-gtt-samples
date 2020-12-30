sap.ui.define(["com/sap/gtt/app/sample/sof/util/AsyncUtils"], function (AsyncUtils) {
  "use strict";

  var sandbox = sinon.sandbox.create();

  QUnit.module("util/AsyncUtils", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("finally - resolve", function (assert) {
    // Arrange
    var done = assert.async();

    // Act
    var promise = Promise.resolve("hello");

    var promise1 = AsyncUtils.finally(promise, function () {});

    promise1.then(function (result) {
      // Assert
      assert.ok(result === "hello", "Finally is handled");
      done();
    });
  });

  QUnit.test("finally - reject", function (assert) {
    // Arrange
    var done = assert.async();

    // Act
    var promise = Promise.reject("failed");

    var promise1 = AsyncUtils.finally(promise, function () {});

    promise1.catch(function (reason) {
      assert.ok(reason === "failed", "Finally is handled");
      done();
    });
  });
});
