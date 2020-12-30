sap.ui.define(["com/sap/gtt/app/sample/sof/util/RestClient"], function (RestClient) {
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

  QUnit.module("util/RestClient", {
    beforeEach: function () {
      this.componentName = "com.sap.gtt.app.sample.sof";
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.skip("fetch", function (assert) {
    // Arrange
    var done = assert.async();

    // Act
    var url = "webapp/manifest.json";
    var promise = RestClient.fetch(url);

    // Assert
    var componentName = this.componentName;
    promise
      .then(function (res) {
        assert.strictEqual(res.status, 200, "status code is correct");
        assert.strictEqual(res.ok, true, "response is ok");
        assert.strictEqual(
          res.headers.get("Content-Type"),
          "application/json",
          "response header Content-Type is correct"
        );

        return res.json();
      })
      .then(function (data) {
        assert.strictEqual(data["sap.app"].id, componentName, "data is correct");
        done();
      });
  });

  QUnit.test("get", function (assert) {
    // Arrange
    var done = assert.async();

    // Act
    var url = "webapp/manifest.json";
    var promise = RestClient.get(url);

    // Assert
    var componentName = this.componentName;
    promise.then(function (body) {
      assert.strictEqual(body["sap.app"].id, componentName, "body is correct");
      done();
    });
  });

  QUnit.test("post", function (assert) {
    // Arrange
    var done = assert.async();

    var csrfToken = "csrfToken";
    stub(RestClient, "getCSRFToken").resolves(csrfToken);

    // Act
    var url = "webapp/manifest.json";
    var data = {
      foo: "post",
    };
    var promise = RestClient.post(url, data);

    // Assert
    var componentName = this.componentName;
    promise.then(function (body) {
      assert.strictEqual(body["sap.app"].id, componentName, "body is correct");
      done();
    });
  });

  QUnit.test("put", function (assert) {
    // Arrange
    var done = assert.async();

    var csrfToken = "csrfToken";
    stub(RestClient, "getCSRFToken").resolves(csrfToken);

    // Act
    var url = "webapp/manifest.json";
    var data = {
      foo: "put",
    };
    var promise = RestClient.put(url, data);

    // Assert
    var componentName = this.componentName;
    promise
      .then(function (body) {
        assert.strictEqual(body["sap.app"].id, componentName, "body is correct");
        done();
      })
      .catch(function (err) {
        assert.ok(err.message, "[Error] " + err.message);
        done();
      });
  });

  QUnit.test("delete", function (assert) {
    // Arrange
    var done = assert.async();

    var csrfToken = "csrfToken";
    stub(RestClient, "getCSRFToken").resolves(csrfToken);

    // Act
    var url = "webapp/manifest.json";
    var promise = RestClient.delete(url);

    // Assert
    promise
      .then(function (body) {
        assert.ok(body, "body should be empty");
        done();
      })
      .catch(function (err) {
        assert.ok(err.message, "[Error] " + err.message);
        done();
      });
  });

  QUnit.test("head", function (assert) {
    // Arrange
    var done = assert.async();

    // Act
    var url = "webapp/manifest.json";
    var promise = RestClient.head(url);

    // Assert
    promise
      .then(function (res) {
        assert.strictEqual(res.data, undefined, "response data should be undefined");
        done();
      })
      .catch(function (err) {
        assert.ok(err.message, "[Error] " + err.message);
        done();
      });
  });

  QUnit.test("setRequestType", function (assert) {
    // Act
    RestClient.setRequestType("type");
    var type = RestClient.getRequestType();

    // Assert
    assert.ok(type === "type", "The request type is set");
  });

  QUnit.test("getCSRFToken", function (assert) {
    var done = assert.async();

    // Arrange
    var fakeProimse = Promise.resolve("token");
    stub(RestClient, "fetchCSRFToken").returns(fakeProimse);

    // Act
    RestClient.getCSRFToken();

    fakeProimse.then(function (token) {
      // Assert
      assert.ok(token === "token", "The csrf token is set");
      done();
    });
  });

  QUnit.test("_fetch", function (assert) {
    assert.expect(0);

    // Act
    RestClient._fetch();
  });

  QUnit.test("fetchCSRFToken", function (assert) {
    var done = assert.async();

    // Arrange
    var fakePromise = Promise.resolve({
      headers: {
        "x-csrf-token": "token",
      },
    });
    stub(RestClient, "head").returns(fakePromise);

    // Act
    RestClient.fetchCSRFToken();

    fakePromise.then(function (res) {
      // Assert
      assert.ok(res.headers["x-csrf-token"] === "token", "The token is got");
      done();
    });
  });
});
