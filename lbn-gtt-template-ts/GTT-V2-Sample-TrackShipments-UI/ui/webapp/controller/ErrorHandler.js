sap.ui.define(
  [
    "sap/ui/base/Object",
    "sap/m/MessageBox",
  ], function (
    UI5Object,
    MessageBox
  ) {
    "use strict";

    /**
     * Handles OData communication errors and displays an error message box when needed.
     * Handling is done by attaching error handlers to the default OData model of the given component.
     *
     * @class
     * @param {sap.ui.core.UIComponent} oComponent Reference to the app's component
     * @public
     * @extends sap.ui.base.Object
     */
    var ErrorHandler = UI5Object.extend("com.sap.gtt.app.sample.sst.ErrorHandler", {
      constructor: function (component) {
        UI5Object.apply(this, arguments);

        /**
         * The main service's OData model
         *
         * @private
         */
        this._model = component.getModel();

        /**
         * Indicates that an MessageBox has been opened
         *
         * @private
         */
        this._messageOpen = false;

        /**
         * The standard error text to be displayed on the MessageBox
         *
         * @private
         */
        this.attachMetadataFailedHandler();
        this.attachBatchRequestCompletedHandler();
        this.attachBatchRequestFailedHandler();
      },
    });

    /**
     * Attaches the handler function to the {@link sap.ui.model.odata.v2.ODataModel} MetadataFailed event
     *
     * @public
     */
    ErrorHandler.prototype.attachMetadataFailedHandler = function () {
      this._model.attachMetadataFailed(function (oEvent) {
        this._metadataFailedHandler(oEvent);
      }.bind(this));
    };

    /**
     * Attaches the handler function to the {@link sap.ui.model.odata.v2.ODataModel} BatchRequestCompleted event
     *
     * @public
     */
    ErrorHandler.prototype.attachBatchRequestCompletedHandler = function () {
      this._model.attachBatchRequestCompleted(function (event) {
        this._batchRequestCompletedHandler(event);
      }.bind(this));
    };

    /**
     * Attaches the handler function to the {@link sap.ui.model.odata.v2.ODataModel} BatchRequestFailed event
     *
     * @public
     */
    ErrorHandler.prototype.attachBatchRequestFailedHandler = function () {
      this._model.attachBatchRequestFailed(function (oEvent) {
        this._batchRequestFailedHandler(oEvent);
      }.bind(this));
    };

    /**
     * Handler function for the {@link sap.ui.model.odata.v2.ODataModel} MetadataFailed event.
     * Displays the MessageBox
     *
     * @param {sap.ui.base.Event} oEvent The event triggering the handler
     * @private
     */
    ErrorHandler.prototype._metadataFailedHandler = function (oEvent) {
      if (oEvent && oEvent.getParameters()) {
        var parameters = oEvent.getParameters();
        var checkResult = this.checkHttpStatus(parameters.statusCode, parameters.statusText, true);
        var responseText = JSON.parse(parameters.responseText);
        var errorMessage = responseText.error.message;
        var detailMessage = "";

        responseText.error.details.forEach(function (detail) {
          detailMessage += detail.message;
        });
        this.displayErrorMessageBox(checkResult.isMissingAuthorization, checkResult.isError, errorMessage, detailMessage);
      }
    };

    /**
     * Handler function for the {@link sap.ui.model.odata.v2.ODataModel} BatchRequestCompleted event.
     * The event is not only triggered when a batch request itself has been accepted for processing
     * (in other words some status code other than 202, e.g. missing authorization 403).
     * Thus, we check if the batch request itself has been accepted.
     * If it has been accepted, the embedded requests could have been failed.
     * Thus, we check if at least one embedded request failed and display the error MessageBox if needed.
     * It is enough to display the error message box; a detailed error message is not necessary.
     * If the batch has not been accepted (e.g. HTTP status 403) we do not check for the embedded requests.
     * In this case we do not do anything in this function because handler function for BatchRequestFailedHandler event will be called afterwards.
     *
     * @param {sap.ui.base.Event} oEvent The event triggering the handler
     * @private
     */
    ErrorHandler.prototype._batchRequestCompletedHandler = function (oEvent) {
      var isCheckEmbeddedRequests = true;
      var response = {};
      if (oEvent) {
        response = oEvent.getParameter("response");
        if (response) {
          var checkResult = this.checkHttpStatus(response.statusCode, response.statusText, false);
          if (checkResult.isMissingAuthorization || checkResult.isError) {
            isCheckEmbeddedRequests = false;
          }
        }
      }
      if (isCheckEmbeddedRequests && oEvent && oEvent.getParameter("requests")) {
        var checkResultEmbeddedRequests = {};
        checkResultEmbeddedRequests = this.checkHttpStatusEmbeddedRequest(oEvent.getParameter("requests"));
        if (checkResultEmbeddedRequests.isMissingAuthorization || checkResultEmbeddedRequests.isError) {
          this.displayErrorMessageBox(checkResultEmbeddedRequests.isMissingAuthorization,
            checkResultEmbeddedRequests.isError,
            checkResultEmbeddedRequests.errorMessage,
            checkResultEmbeddedRequests.detailMessage);
        }
      }
    };

    /**
     * Handler function for the {@link sap.ui.model.odata.v2.ODataModel} BatchRequestFailed event.
     * The event is triggered when a batch request itself fails (status code !== 202).
     * It is enough to display the error message box; a detailed error message is not necessary.
     *
     * @param {sap.ui.base.Event} oEvent The event triggering the handler
     * @private
     */
    ErrorHandler.prototype._batchRequestFailedHandler = function (oEvent) {
      if (oEvent) {
        var response = oEvent.getParameter("response");

        if (response) {
          var checkResult = this.checkHttpStatus(response.statusCode, response.statusText, false);
          if (response.statusCode === 504 || response.statusCode === 401) {
            this.displayErrorMessageBox(false, true, response.message);
          } else {
            var responseText = JSON.parse(response.responseText);
            var errorMessage = responseText.error.message.value + "\n";
            var detailMessage = "";
            if (responseText.error.details) {
              responseText.error.details.forEach(function (detail) {
                detailMessage += detail.message + "\n";
              });
            }
            this.displayErrorMessageBox(checkResult.isMissingAuthorization, checkResult.isError, errorMessage, detailMessage);
          }
        }
      }
    };

    /**
     * Checks HTTP status code and text for missing authorization and errors.
     * Note: The UI5 framework can abort pending requests (e.g. because it has become obsolete in the meantime).
     *       An aborted request has statusCode 0.
     *       We must not treat this as an error.
     *
     * @param {Integer} httpStatusCode The HTTP status code
     * @param {String} httpStatusText The HTTP status text
     * @param {Boolean} isIgnoreResourceNotFound If true, HTTP status 404 with status text "Resource not found" will not be treated as error
     * @return {object}  The result object
     * @return {object.isMissingAuthorization} True if HTTP status indicates missing authorization (HTTP status 401 or 403)
     * @return {object.isError} True if HTTP status indicates error. Not true for authorization issues.
     * @public
     */
    ErrorHandler.prototype.checkHttpStatus = function (httpStatusCode, httpStatusText, isIgnoreResourceNotFound) {
      var result = {
        isMissingAuthorization: false,
        isError: false,
      };
      var statusCode = "" + httpStatusCode; // Type cast
      var statusText = "" + httpStatusText; // Type cast
      if (!statusCode.match(/^2(.*)/) && statusCode !== "0") {
        if (statusCode === "401" || statusCode === "403") {
          result.isMissingAuthorization = true;
        } else if ((isIgnoreResourceNotFound === false) && statusCode === "404" && (statusText.indexOf("Resource not found") !== -1)) {
          result.isError = true;
        } else {
          result.isError = true;
        }
      }
      return result;
    };

    /**
     * Check if at least one embedded request of a batch request failed.
     *
     * @param {object[]} requests Xhr requests.
     * @return {object} The result object
     * @return {object.isMissingAuthorization} True if HTTP status indicates missing authorization (HTTP status 401 or 403)
     * @return {object.isError} True if HTTP status indicates error. Not true for authorization issues.
     * @public
     */
    ErrorHandler.prototype.checkHttpStatusEmbeddedRequest = function (requests) {
      var result = {
        isMissingAuthorization: false,
        isError: false,
        errorMessage: "",
        detailMessage: "",
      };
      requests.forEach(function (request) {
        var response = request.response;
        var responseText = response.responseText;
        if (responseText) {
          var errorMessage = JSON.parse(responseText).error;
          result.errorMessage += errorMessage.message.value + "\n";
          if (errorMessage.details) {
            errorMessage.details.forEach(function (detail) {
              result.detailMessage += detail.message + "\n";
            });
          }
        }
        var resultHelper = this.checkHttpStatus(response.statusCode, response.statusText, false);
        if (resultHelper.isMissingAuthorization) {
          result.isMissingAuthorization = true;
        } else if (resultHelper.isError) {
          result.isError = true;
        }
      }.bind(this));
      return result;
    };

    /**
     * Displays a {@link sap.m.MessageBox} with an error message when a service call has been failed.
     * The {@link sap.ui.model.odata.v2.ODataModel} BatchRequestFailed event will be fired once per batch group.
     * If more than one batch group have been submitted at once, the MessageBox will be displayed for the
     * first batch group, only.
     *
     * @param {Boolean} isMissingAuthorization True if HTTP status indicates missing authorization (HTTP status 401 or 403)
     * @param {Boolean} isError True if HTTP status indicates error. Not true for authorization issues.
     * @param {String} errorMessage The error message.
     * @param {String} detailMessage The detail error message.
     * @private
     */
    ErrorHandler.prototype.displayErrorMessageBox = function (isMissingAuthorization, isError, errorMessage, detailMessage) {
      if (this._messageOpen) {
        return;
      }
      if (isMissingAuthorization) {
        MessageBox.show(errorMessage, {
          onClose: function () {
            this._messageOpen = false;
          }.bind(this),
        });
        this._messageOpen = true;
      } else if (isError) {
        MessageBox.error(errorMessage, {
          details: detailMessage,
          onClose: function () {
            this._messageOpen = false;
          }.bind(this),
        });
        this._messageOpen = true;
      }
    };

    return ErrorHandler;
  }
);
