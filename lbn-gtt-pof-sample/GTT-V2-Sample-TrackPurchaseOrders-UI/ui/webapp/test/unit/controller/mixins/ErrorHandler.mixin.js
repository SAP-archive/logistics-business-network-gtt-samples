sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/mixins/ErrorHandler.mixin",
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

  QUnit.module("com.sap.gtt.app.sample.pof.controller.mixins.ErrorHandler.mixin", {
    beforeEach: function () {
      var fakeModel = {};
      stub(fakeModel, "attachBatchRequestCompleted");
      stub(fakeModel, "attachBatchRequestFailed");

      var component = {};
      stub(component, "getModel").returns(fakeModel);

      this.oErrorHandler = ErrorHandler;
    },
    afterEach: function () {
      sandbox.restore();
      this.oErrorHandler = null;
    },
  });

  QUnit.test("setComponent: component exists", function (assert) {
    // Arrange
    var oComponent = {};

    // Assert
    var oResult = this.oErrorHandler.setComponent(oComponent);
    assert.ok(!oResult, "Return undefined");
    assert.ok(!!this.oErrorHandler._oComponent, "Component was set");
  });

  QUnit.test("getODataModel: component exists", function (assert) {
    // Arrange
    var oComponent = {
      getModel: function () {
        return "success";
      },
    };

    // Assert
    this.oErrorHandler._oComponent = oComponent;
    var sResult = this.oErrorHandler.getODataModel();

    assert.ok(sResult === "success", "Component was set");
  });

  QUnit.test("handleError: oData is not an object", function (assert) {
    // Arrange
    var oData = "{}";
    stub(MessageBox, "error");

    // Assert
    var oResult = this.oErrorHandler.handleError(oData);
    assert.ok(!oResult, "Return undefined");
    assert.ok(MessageBox.error.calledOnce, "Show Message box");
  });

  QUnit.test("handleError: oData is not an object", function (assert) {
    // Arrange
    var oData = {};
    stub(MessageBox, "error");

    // Assert
    var oResult = this.oErrorHandler.handleError(oData);
    assert.ok(!oResult, "Return undefined");
  });

  QUnit.test("handleError: oData is an object with error details", function (assert) {
    // Arrange
    var oData = {
      error: {
        details: [{
          message: "Error details",
        }],
        message: "Error details",
      },
    };
    stub(MessageBox, "error");

    // Assert
    var oResult = this.oErrorHandler.handleError(oData);
    assert.ok(!oResult, "Show Message box with error is oData is object and has error details");
    assert.ok(MessageBox.error.calledOnce, "Show Message box");
    assert.ok(MessageBox.error.calledWith("Error details", {details: "Error details"}), "Show Message box with 'Error details' message and details array");
  });

  QUnit.test("handleError: oData is an object without error details", function (assert) {
    // Arrange
    var oData = {
      error: {
        details: [],
        message: "Error details",
      },
    };
    stub(MessageBox, "error");

    // Assert
    var oResult = this.oErrorHandler.handleError(oData);
    assert.ok(!oResult, "Show Message box with error is oData is object without error details");
    assert.ok(MessageBox.error.calledWith("Error details"), "Show Message box with 'Error details' message");
  });

  QUnit.test("checkHttpStatus: HTTP status code 200", function (assert) {
    // Arrange
    var sStatusCode = 200;
    var sStatusText = "Success";

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatus(sStatusCode, sStatusText);
    assert.ok(!oResult.isError, "Status code 200 - no errors");
    assert.ok(!oResult.isMissingAuthorization, "Status code 200 - authorization doesn't have errors");
  });

  QUnit.test("checkHttpStatus: HTTP status code 400", function (assert) {
    // Arrange
    var sStatusCode = 400;
    var sStatusText = "Bad Request";

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatus(sStatusCode, sStatusText);
    assert.ok(oResult.isError, "Status code 400 - error");
    assert.ok(!oResult.isMissingAuthorization, "Status code 400 - authorization doesn't have errors");
  });

  QUnit.test("checkHttpStatus: HTTP status code 401", function (assert) {
    // Arrange
    var sStatusCode = 401;
    var sStatusText = "Unauthorized";

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatus(sStatusCode, sStatusText);
    assert.ok(!oResult.isError, "Status code 401 - no error");
    assert.ok(oResult.isMissingAuthorization, "Status code 401 - missing authorization");
  });

  QUnit.test("checkHttpStatus: HTTP status code 403", function (assert) {
    // Arrange
    var sStatusCode = 403;
    var sStatusText = "Forbidden";

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatus(sStatusCode, sStatusText);
    assert.ok(!oResult.isError, "Status code 403 - error");
    assert.ok(oResult.isMissingAuthorization, "Status code 403 - missing authorization");
  });

  QUnit.test("checkHttpStatus: HTTP status code 404", function (assert) {
    // Arrange
    var sStatusCode = 404;
    var sStatusText = "Resource not found";
    var bIgnoreResNotFound = true;

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatus(sStatusCode, sStatusText, bIgnoreResNotFound);
    assert.ok(oResult.isError, "Status code 404 - error");
    assert.ok(!oResult.isMissingAuthorization, "Status code 404 - authorization doesn't have errors");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 200", function (assert) {
    // Arrange
    var aRequests = [{
      response: {
        statusCode: "200",
      },
    }];

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(!oResult.isError, "Status code 200 - No error");
    assert.ok(!oResult.isMissingAuthorization, "Status code 200 - No missing authorization");
    assert.ok(!oResult.errorMessage.length, "Status code 200 - No error message");
    assert.ok(oResult.detailMessage.match(""), "Status code 200 - No detail message");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 400", function (assert) {
    // Arrange
    var aRequests = [{
      response: {
        statusCode: "400",
        statusText: "Bad Request",
      },
    }];

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(oResult.isError, "Status code 400 - error: Bad Request");
    assert.ok(!oResult.isMissingAuthorization, "Status code 400 - authorization doesn't have errors");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 401", function (assert) {
    // Arrange
    var aRequests = [{
      response: {
        statusCode: "401",
        statusText: "Unauthorized",
      },
    }];

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(!oResult.isError, "Status code 401 - no error");
    assert.ok(oResult.isMissingAuthorization, "Status code 401 - missing authorization: Unauthorized");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 403", function (assert) {
    // Arrange
    var aRequests = [{
      response: {
        statusCode: "403",
        statusText: "Forbidden",
      },
    }];

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(!oResult.isError, "Status code 403 - no error");
    assert.ok(oResult.isMissingAuthorization, "Status code 403 - missing authorization: Forbidden");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 404 and status 'text Resource not found'", function (assert) {
    // Arrange
    var aRequests = [{
      response: {
        statusCode: "404",
        statusText: "Resource not found",
      },
    }];

    // Assert
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(oResult.isError, "Status code 404 - error: Resource not found");
    assert.ok(!oResult.isMissingAuthorization, "Status code 404 - authorization doesn't have errors");
  });

  QUnit.test("checkHttpStatusEmbeddedRequest: HTTP status code 500", function (assert) {
    // Arrange
    var aRequests = [{
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
    var oResult = this.oErrorHandler.checkHttpStatusEmbeddedRequest(aRequests);
    assert.ok(oResult.isError, "Status codes 200, 500 - error");
    assert.ok(!oResult.isMissingAuthorization, "Status codes 200, 500 - authorization doesn't have errors");
  });

  QUnit.test("updateResult: HTTP status code 500", function (assert) {
    // Arrange
    var aRequests = {
      response: {
        statusCode: "200",
        statusText: "Ok",
      },
    };

    // Assert
    var oResult = this.oErrorHandler.updateResult(aRequests);
    assert.ok(!oResult, "Return undefined, no errors");
  });

  QUnit.test("updateResult: HTTP status code 500", function (assert) {
    // Arrange
    var oRequest = {
      response: {
        statusCode: "500",
        statusText: "Error",
        responseText: '{"error": {"message": {"value": "500 - error"}, "details": [{"message": "detail"}]}}',
      },
    };
    var oErrorsConfig = {
      isMissingAuthorization: false,
      isError: false,
      errorMessage: [],
      detailMessage: "",
    };

    // Assert
    var oResult = this.oErrorHandler.updateResult(oRequest, oErrorsConfig);
    assert.ok(!oResult, "Return undefined, errors are here");
    assert.ok(oErrorsConfig.errorMessage.length === 1, "1 error message");
    assert.ok(oErrorsConfig.detailMessage === "detail\n", "1 detail error message");
  });

  QUnit.test("displayErrorMessageBox", function (assert) {
    // message is already opened
    this.oErrorHandler._messageOpen = true;
    var oResult = this.oErrorHandler.displayErrorMessageBox();
    assert.ok(!oResult, "Return undefined");
    assert.ok(this.oErrorHandler._messageOpen === true, "The Message Box opens");
  });

  QUnit.test("displayErrorMessageBox", function (assert) {
    // missing authorization
    var bIsMissingAuthorization = true;
    stub(MessageBox, "show");

    this.oErrorHandler._messageOpen = false;
    this.oErrorHandler.displayErrorMessageBox(bIsMissingAuthorization);

    assert.ok(MessageBox.show.calledOnce, "MessageBox show is called once");
    assert.ok(this.oErrorHandler._messageOpen === true, "The Message Box opens");
  });

  QUnit.test("displayErrorMessageBox", function (assert) {
    // authorization is correct and error exists
    stub(MessageBox, "error");

    this.oErrorHandler._messageOpen = false;
    this.oErrorHandler.displayErrorMessageBox(false, true);

    assert.ok(MessageBox.error.calledOnce, "MessageBox error is called once");
    assert.ok(this.oErrorHandler._messageOpen === true, "The Message Box opens");
  });

  QUnit.test("_batchRequestFailedHandler: Error with status 401", function (assert) {
    // Arrange
    var oFakeCheckHTTPStatusResult = {
      isMissingAuthorization: false,
      isError: true,
    };
    stub(this.oErrorHandler, "checkHttpStatus").returns(oFakeCheckHTTPStatusResult);
    stub(this.oErrorHandler, "displayErrorMessageBox");
    var oFakeEvent = new Event("", "", {
      response: {
        statusCode: 401,
        statusText: "Unauthorized",
        message: "Authorization error",
      },
    });

    // Act
    this.oErrorHandler._batchRequestFailedHandler(oFakeEvent);

    // Assert
    assert.ok(this.oErrorHandler.displayErrorMessageBox.calledOnce, "Method to display the error message box has called once");
  });

  QUnit.test("_batchRequestFailedHandler: Error with status 504", function (assert) {
    // Arrange
    var oFakeCheckHTTPStatusResult = {
      isMissingAuthorization: false,
      isError: true,
    };
    stub(this.oErrorHandler, "checkHttpStatus").returns(oFakeCheckHTTPStatusResult);
    stub(this.oErrorHandler, "displayErrorMessageBox");
    var oFakeEvent = new Event("", "", {
      response: {
        message: "error",
        statusCode: 504,
        statusText: "Internal server error",
        responseText: '{"error": {"message": {"value": "504 - Internal server error"}, "details": [{"message": "detail"}]}}',
      },
    });

    // Act
    this.oErrorHandler._batchRequestFailedHandler(oFakeEvent);

    // Assert
    assert.ok(this.oErrorHandler.displayErrorMessageBox.calledOnce, "displayErrorMessageBox method has called once");
    assert.ok(this.oErrorHandler.displayErrorMessageBox.calledWith(false, true), "displayErrorMessageBox method is called with error");
  });

  QUnit.test("_batchRequestCompletedHandler: Error with status 504", function (assert) {
    // Arrange
    var oFakeCheckHTTPStatusResult = {
      isMissingAuthorization: false,
      isError: true,
    };
    stub(this.oErrorHandler, "checkHttpStatus").returns(oFakeCheckHTTPStatusResult);
    var oFakeEvent = new Event("", "", {
      response: {
        message: "error",
        statusCode: 504,
        statusText: "Internal server error",
        responseText: '{"error": {"message": {"value": "504 - Internal server error"}, "details": [{"message": "detail"}]}}',
      },
    });

    // Act
    this.oErrorHandler._batchRequestCompletedHandler(oFakeEvent);

    // Assert
    assert.ok(this.oErrorHandler.checkHttpStatus.calledOnce, "checkHttpStatus method has called once");
  });

  QUnit.test("_batchRequestCompletedHandler: Error with status 504", function (assert) {
    // Arrange
    var oFakeCheckHTTPStatusResult = {
      isMissingAuthorization: false,
      isError: true,
      errorMessage: ["Internal server error"],
      detailMessage: "Internal server error",
    };
    stub(this.oErrorHandler, "checkHttpStatusEmbeddedRequest").returns(oFakeCheckHTTPStatusResult);
    stub(this.oErrorHandler, "displayErrorMessageBox");
    var oFakeEvent = new Event("", "", {
      requests: [
        {
          response: {
            message: "error",
            statusCode: 504,
            statusText: "Internal server error",
          },
        },
      ],
    });

    // Act
    this.oErrorHandler._batchRequestCompletedHandler(oFakeEvent);

    // Assert
    assert.ok(this.oErrorHandler.displayErrorMessageBox.calledOnce, "displayErrorMessageBox method has called once");
    assert.ok(this.oErrorHandler.checkHttpStatusEmbeddedRequest.calledOnce, "checkHttpStatusEmbeddedRequest method has called once");
  });
});
