sap.ui.define(["com/sap/gtt/app/sample/pof/util/RestClient"], function (RestClient) {
  "use strict";

  // var sandbox = sinon.sandbox.create();

  function stub(object, method, func) {
    if (!(method in object)) {
      object[method] = function () {};
    }

    var stubbed = sinon.stub(object, method);

    if (typeof func === "function") {
      return stubbed.callsFake(func);
    }

    return stubbed;
  }

  QUnit.module("util/RestClient", {
    beforeEach: function () {
      this.componentName = "com.sap.gtt.app.sample.pof";
    },
    afterEach: function () {
      sinon.restore();
    },
  });

  // QUnit.skip("fetch", function (assert) {
  //   // Arrange
  //   var done = assert.async();

  //   // Act
  //   var url = "webapp/manifest.json";
  //   var promise = RestClient.fetch(url);

  //   // Assert
  //   var componentName = this.componentName;
  //   promise
  //     .then(function (res) {
  //       assert.strictEqual(res.status, 200, "status code is correct");
  //       assert.strictEqual(res.ok, true, "response is ok");
  //       assert.strictEqual(
  //         res.headers.get("Content-Type"),
  //         "application/json",
  //         "response header Content-Type is correct"
  //       );

  //       return res.json();
  //     })
  //     .then(function (data) {
  //       assert.strictEqual(data["sap.app"].id, componentName, "data is correct");
  //       done();
  //     });
  // });

  QUnit.test("get", function (assert) {
    // Arrange
    var done = assert.async();
    var oFakeResult = {
      data: "body",
    };
    var oFakePromise = new Promise(function (resolve, reject) {
      resolve(oFakeResult);
    });
    // Act
    var sUrl = "webapp/manifest.json";
    stub(RestClient, "request").returns(oFakePromise);
    var oPromise = RestClient.get(sUrl);

    // Assert
    oPromise.then(function (sBody) {
      assert.ok(sBody === "body", "body is correct");
      done();
    });
  });

  QUnit.test("request - fetch", function (assert) {
    // Arrange
    var done = assert.async();
    var sReqType = "fetch";
    var oConfig = {
      headers: "headers",
    };
    var oFakePromise = new Promise(function (resolve, reject) {
      resolve("success");
    });
    // Act
    var sUrl = "webapp/manifest.json";
    stub(RestClient, "getRequestType").returns(sReqType);
    stub(RestClient, "_fetch").returns(oFakePromise);
    stub(RestClient, "_handleRequestHeaders").returns(oConfig);
    var oPromise = RestClient.request(sUrl);

    // Assert
    oPromise.then(function (sResult) {
      assert.ok(sResult === "success", "return 'success' data");
      // assert.ok(RestClient._handleRequestHeaders.calledOnce, "return 'success' data");
      done();
    });
  });

  // QUnit.test("request", function (assert) {
  //   // Arrange
  //   var done = assert.async();
  //   var oConfig = {
  //     headers: "headers",
  //   };
  //   var oFakePromise = new Promise(function (resolve, reject) {
  //     resolve("success");
  //   });
  //   var sUrl = "webapp/manifest.json";
  //   // Act
  //   stub(RestClient, "getRequestType").returns();
  //   stub(RestClient, "_jqXhr").returns(oFakePromise);
  //   stub(window, "_handleRequestHeaders").returns(oConfig);
  //   var oPromise = RestClient.request(sUrl);

  //   // Assert
  //   oPromise.then(function (sResult) {
  //     assert.ok(sResult === "success", "return 'success' data");
  //     assert.ok(window._handleRequestHeaders.calledOnce, "return 'success' data");
  //     done();
  //   });
  // });

  // QUnit.skip("post", function (assert) {
  //   // Arrange
  //   var done = assert.async();

  //   var csrfToken = "csrfToken";
  //   stub(RestClient, "getCSRFToken").resolves(csrfToken);

  //   // Act
  //   var sUrl = "../../manifest.json";
  //   var data = {
  //     foo: "post",
  //   };
  //   var oPromise = RestClient.post(sUrl, data);

  //   // Assert
  //   var sComponentName = this.componentName;
  //   oPromise.then(function (body) {
  //     assert.strictEqual(body["sap.app"].id, sComponentName, "body is correct");
  //     done();
  //   });
  // });

  // QUnit.skip("put", function (assert) {
  //   // Arrange
  //   var done = assert.async();

  //   var csrfToken = "csrfToken";
  //   stub(RestClient, "getCSRFToken").resolves(csrfToken);

  //   // Act
  //   var url = "../../manifest.json";
  //   var data = {
  //     foo: "put",
  //   };
  //   var promise = RestClient.put(url, data);

  //   // Assert
  //   var componentName = this.componentName;
  //   promise
  //     .then(function (body) {
  //       assert.strictEqual(body["sap.app"].id, componentName, "body is correct");
  //       done();
  //     })
  //     .catch(function (err) {
  //       assert.ok(err.message, "[Error] " + err.message);
  //       done();
  //     });
  // });

  // QUnit.skip("delete", function (assert) {
  //   // Arrange
  //   var done = assert.async();

  //   var csrfToken = "csrfToken";
  //   stub(RestClient, "getCSRFToken").resolves(csrfToken);

  //   // Act
  //   var url = "webapp/manifest.json";
  //   var promise = RestClient.delete(url);

  //   // Assert
  //   promise
  //     .then(function (body) {
  //       assert.ok(body, "body should be empty");
  //       done();
  //     })
  //     .catch(function (err) {
  //       assert.ok(err.message, "[Error] " + err.message);
  //       done();
  //     });
  // });

  // QUnit.test("head", function (assert) {
  //   // Arrange
  //   var done = assert.async();

  //   // Act
  //   var url = "../../manifest.json";
  //   var promise = RestClient.head(url);

  //   // Assert
  //   promise
  //     .then(function (res) {
  //       assert.strictEqual(res.data, undefined, "response data should be undefined");
  //       done();
  //     })
  //     .catch(function (err) {
  //       assert.ok(err.message, "[Error] " + err.message);
  //       done();
  //     });
  // });

  // QUnit.test("setRequestType", function (assert) {
  //   // Act
  //   RestClient.setRequestType("type");
  //   var type = RestClient.getRequestType();

  //   // Assert
  //   assert.ok(type === "type", "The request type is set");
  // });

  // QUnit.test("getCSRFToken", function (assert) {
  //   var done = assert.async();

  //   // Arrange
  //   var fakeProimse = Promise.resolve("token");
  //   stub(RestClient, "fetchCSRFToken").returns(fakeProimse);

  //   // Act
  //   RestClient.getCSRFToken();

  //   fakeProimse.then(function (token) {
  //     // Assert
  //     assert.ok(token === "token", "The csrf token is set");
  //     done();
  //   });
  // });

  // QUnit.test("_fetch", function (assert) {
  //   assert.expect(0);

  //   // Act
  //   RestClient._fetch();
  // });

  // QUnit.test("fetchCSRFToken", function (assert) {
  //   var done = assert.async();

  //   // Arrange
  //   var fakePromise = Promise.resolve({
  //     headers: {
  //       "x-csrf-token": "token",
  //     },
  //   });
  //   stub(RestClient, "head").returns(fakePromise);

  //   // Act
  //   RestClient.fetchCSRFToken();

  //   fakePromise.then(function (res) {
  //     // Assert
  //     assert.ok(res.headers["x-csrf-token"] === "token", "The token is got");
  //     done();
  //   });
  // });
});
