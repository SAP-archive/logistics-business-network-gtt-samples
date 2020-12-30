sap.ui.define([
  "com/sap/gtt/app/sample/sst/controller/ErrorHandler",
  "sap/m/MessageBox",
  "sap/ui/base/Event",
], function (ErrorHandler, MessageBox, Event) {
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

  QUnit.module("com.sap.gtt.app.sample.sst.controller.ErrorHandler", {
    beforeEach: function () {
      var fakeModel = {};
      stub(fakeModel, "attachMetadataFailed");
      stub(fakeModel, "attachBatchRequestCompleted");
      stub(fakeModel, "attachBatchRequestFailed");

      var component = {};
      stub(component, "getModel").returns(fakeModel);

      this.errorHandler = new ErrorHandler(component);
    },
    afterEach: function () {
      sandbox.restore();
      this.errorHandler.destroy();
    },
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 200", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "200",
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(!result.isError, "No error for status code 200");
    assert.ok(!result.isMissingAuthorization, "No missing authorization for status code 200");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 401", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "401",
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(!result.isError, "No error for status code 401");
    assert.ok(result.isMissingAuthorization, "Missing authorization for status code 401");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 403", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "403",
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(!result.isError, "No error for status code 403");
    assert.ok(result.isMissingAuthorization, "Missing authorization for status code 403");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 404 and status 'text Resource not found'", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "404",
        statusText: "Resource not found",
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(result.isError, "No error for status code 404 and status text 'Resource not found'");
    assert.ok(!result.isMissingAuthorization,
      "No missing authorization for status code 404 and status 'text Resource not found'");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 404", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "404",
        statusText: "Not found",
        responseText: JSON.stringify({error: {details: [{message: "error details"}], message: "error"}}),
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(result.isError, "Error for status code 404");
    assert.ok(!result.isMissingAuthorization, "No missing authorization for status code 404");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 500", function (assert) {
    // Arrange
    var requests = [{
      response: {
        statusCode: "200",
        statusText: "Ok",
      },
    }, {
      response: {
        statusCode: "500",
        statusText: "Internal server error",
      },
    }];

    // Assert
    var result = this.errorHandler.checkHttpStatusEmbeddedRequest(requests);
    assert.ok(result.isError, "Error for status code 500");
    assert.ok(!result.isMissingAuthorization, "No missing authorization for status code 500");
  });

  QUnit.test("_metadataFailedHandler", function (assert) {
    // Arrange
    stub(this.errorHandler, "displayErrorMessageBox");
    var fakeEvent = new Event("", "", {
      request: {
        headers: {
          "x-vcap-request-id": "ce7bd54d-8621-4e1d-6116-f1f47878ff30",
        },
      },
      statusCode: 500,
      statusText: "Internal server error",
      responseText: JSON.stringify({error: {details: [{message: "error details"}], message: "error"}}),
    });

    // Act
    this.errorHandler._metadataFailedHandler(fakeEvent);

    // Assert
    assert.ok(this.errorHandler.displayErrorMessageBox.calledOnce, "Method to display error message box has been called");
  });

  QUnit.test("_batchRequestCompletedHandler: Error in response", function (assert) {
    // Arrange
    stub(this.errorHandler, "displayErrorMessageBox");
    var fakeEvent = new Event("", "", {
      response: {
        statusCode: 500,
        statusText: "Internal server error",
      },
    });

    // Act
    this.errorHandler._batchRequestCompletedHandler(fakeEvent);

    // Assert
    assert.notOk(this.errorHandler.displayErrorMessageBox.calledOnce, "Method to display the error message box has not been called");
  });

  QUnit.test("_batchRequestCompletedHandler: Error in embedded request", function (assert) {
    // Arrange
    stub(this.errorHandler, "displayErrorMessageBox");
    var fakeEvent = new Event("", "", {
      headers: {
        "x-vcap-request-id": "ce7bd54d-8621-4e1d-6116-f1f47878ff30",
      },
      response: {
        statusCode: 200,
        statusText: "Ok",
      },
      requests: [{
        response: {
          statusCode: 500,
          statusText: "Internal server error",
        },
      }],
    });

    // Act
    this.errorHandler._batchRequestCompletedHandler(fakeEvent);

    // Assert
    assert.ok(this.errorHandler.displayErrorMessageBox.calledOnce, "Method to display the error message box has been called");
  });

  QUnit.test("_batchRequestFailedHandler: With response", function (assert) {
    // Arrange
    stub(this.errorHandler, "displayErrorMessageBox");
    var fakeEvent = new Event("", "", {
      response: {
        statusCode: 500,
        statusText: "Internal server error",
        responseText: JSON.stringify({error: {details: [{message: "error details"}], message: "error"}}),
      },
      request: {
        headers: {
          "x-vcap-request-id": "ce7bd54d-8621-4e1d-6116-f1f47878ff30",
        },
      },
    });

    // Act
    this.errorHandler._batchRequestFailedHandler(fakeEvent);

    // Assert
    assert.ok(this.errorHandler.displayErrorMessageBox.calledOnce, "Method to display the error message box has been called");
  });

  QUnit.test("_batchRequestFailedHandler: 504", function (assert) {
    // Arrange
    stub(this.errorHandler, "displayErrorMessageBox");
    var fakeEvent = new Event("", "", {
      response: {
        message: "error",
        statusCode: 504,
        statusText: "Internal server error",
      },
    });

    // Act
    this.errorHandler._batchRequestFailedHandler(fakeEvent);

    // Assert
    assert.ok(this.errorHandler.displayErrorMessageBox.calledOnce, "Method to display the error message box has been called");
  });

  QUnit.test("displayErrorMessageBox", function (assert) {
    this.errorHandler._messageOpen = true;
    this.errorHandler.displayErrorMessageBox();

    // Arrange
    stub(MessageBox, "show");
    stub(MessageBox, "error");
    this.errorHandler._messageOpen = false;
    this.errorHandler.displayErrorMessageBox(true);

    this.errorHandler._messageOpen = false;
    this.errorHandler.displayErrorMessageBox(false, true);

    // Asesrt
    assert.ok(this.errorHandler._messageOpen === true, "The Message Box opens");

    MessageBox.show.firstCall.args[1].onClose();
    assert.ok(this.errorHandler._messageOpen === false, "The Message Box closes");

    this.errorHandler._messageOpen = true;
    MessageBox.error.firstCall.args[1].onClose();
    assert.ok(this.errorHandler._messageOpen === false, "The Message Box closes");
  });

  QUnit.test("Call attach handlers", function (assert) {
    assert.expect(0);

    // Arrange
    var fakeModel = this.errorHandler._model;
    stub(this.errorHandler, "_metadataFailedHandler");
    stub(this.errorHandler, "_batchRequestFailedHandler");

    // Act
    this.errorHandler.attachMetadataFailedHandler();
    this.errorHandler.attachBatchRequestFailedHandler();

    fakeModel.attachMetadataFailed.firstCall.args[0]();
    fakeModel.attachBatchRequestFailed.firstCall.args[0]();
  });
});
