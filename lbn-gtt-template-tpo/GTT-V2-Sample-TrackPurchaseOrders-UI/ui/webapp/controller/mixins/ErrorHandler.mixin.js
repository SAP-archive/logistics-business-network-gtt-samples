sap.ui.define([
  "sap/base/util/isPlainObject",
  "sap/m/MessageBox",
], function (isPlainObject, MessageBox) {
  "use strict";

  return {
    _messageOpen: false,

    setComponent: function (oComponent) {
      this._oComponent = oComponent;
    },

    initErrorHandler: function () {
      this._attachBatchRequestCompletedHandler();
      this._attachBatchRequestFailedHandler();
    },

    getODataModel: function () {
      return this._oComponent.getModel();
    },

    handleServerError: function (oError) {
      var oResponse = oError.response;
      var oData = oResponse.data;
      this.handleError(oData);
    },

    handleError: function (oData) {
      if (!isPlainObject(oData)) {
        MessageBox.error(oData);
        return;
      }

      if (oData.error) {
        var oError = oData.error;
        var oMessageErrorBoxConfig = {};
        var aMessages = (oError.details || []).map(function (oDetail) {
          return oDetail.message;
        });
        if (aMessages.length) {
          oMessageErrorBoxConfig = {
            details: aMessages.join("<br>"),
          };
        } else if (oError.rootCauseMessage) {
          oMessageErrorBoxConfig = {
            details: oError.rootCauseMessage,
            contentWidth: "100px",
            styleClass: "",
          };
        }
        MessageBox.error(oError.message, oMessageErrorBoxConfig);
      }
    },

    checkHttpStatus: function (sHTTPStatusCode, sHTTPStatusText, bIsIgnoreResourceNotFound) {
      var oResult = {
        isMissingAuthorization: false,
        isError: false,
      };
      var sStatusCode = "" + sHTTPStatusCode; // Type cast
      var sStatusText = "" + sHTTPStatusText; // Type cast
      if (!sStatusCode.match(/^2(.*)/) && sStatusCode !== "0") {
        if (sStatusCode === "401" || sStatusCode === "403") {
          oResult.isMissingAuthorization = true;
        } else if ((bIsIgnoreResourceNotFound === false) && sStatusCode === "404" && (sStatusText.indexOf("Resource not found") !== -1)) {
          oResult.isError = true;
        } else {
          oResult.isError = true;
        }
      }
      return oResult;
    },

    checkHttpStatusEmbeddedRequest: function (aRequests) {
      var oResult = {
        isMissingAuthorization: false,
        isError: false,
        errorMessage: [],
        detailMessage: "",
      };
      if (aRequests) {
        aRequests.forEach(function (oRequest) {
          this.updateResult(oRequest, oResult);

          var oResultHelper = this.checkHttpStatus(oRequest.response.statusCode, oRequest.response.statusText, false);
          if (oResultHelper.isMissingAuthorization) {
            oResult.isMissingAuthorization = true;
          } else if (oResultHelper.isError) {
            oResult.isError = true;
          }

        }.bind(this));
      }
      return oResult;
    },

    updateResult: function (oRequest, oResult) {
      var oResponse = oRequest.response;
      var sResponseText = oResponse.responseText;
      if (sResponseText) {
        var oErrorMessage = JSON.parse(sResponseText).error,
          sErrorMessage = oErrorMessage.message.value || oErrorMessage.message + "\n";
        if (!~oResult.errorMessage.indexOf(sErrorMessage)) {
          oResult.errorMessage.push(sErrorMessage);
        }
        if (oErrorMessage.details) {
          oErrorMessage.details.forEach(function (detail) {
            oResult.detailMessage += detail.message + "\n";
          });
        } else if (oErrorMessage.rootCauseMessage) {
          oResult.detailMessage += oErrorMessage.rootCauseMessage + "\n";
        }
      }
    },

    displayErrorMessageBox: function (bIsMissingAuthorization, bIsError, sErrorMessage, sDetailMessage) {
      if (this._messageOpen) {
        return;
      }
      if (bIsMissingAuthorization) {
        MessageBox.show(sErrorMessage, {
          onClose: function () {
            this._messageOpen = false;
          }.bind(this),
        });
        this._messageOpen = true;
      } else if (bIsError) {
        MessageBox.error(sErrorMessage, {
          details: sDetailMessage,
          contentWidth: "100px",
          styleClass: "",
          onClose: function () {
            this._messageOpen = false;
          }.bind(this),
        });
        this._messageOpen = true;
      }
    },

    _attachBatchRequestCompletedHandler: function () {
      this.getODataModel().attachBatchRequestCompleted(function (event) {
        this._batchRequestCompletedHandler(event);
      }.bind(this));
    },

    _attachBatchRequestFailedHandler: function () {
      this.getODataModel().attachBatchRequestFailed(function (oEvent) {
        this._batchRequestFailedHandler(oEvent);
      }.bind(this));
    },

    _batchRequestFailedHandler: function (oEvent) {
      if (oEvent) {
        var oResponse = oEvent.getParameter("response");
        if (oResponse) {
          var oCheckResult = this.checkHttpStatus(oResponse.statusCode, oResponse.statusText, false);
          if (oResponse.statusCode === 504 || oResponse.statusCode === 401) {
            this.displayErrorMessageBox(false, true, oResponse.message);
          } else {
            var oResponseText = JSON.parse(oResponse.responseText);
            var sErrorMessage = oResponseText.error.message.value + "\n";
            var sDetailMessage = "";
            if (oResponseText.error.details) {
              oResponseText.error.details.forEach(function (detail) {
                sDetailMessage += detail.message + "\n";
              });
            } else if (oResponse.error.rootCauseMessage) {
              sDetailMessage = oResponse.error.rootCauseMessage + "\n";
            }
            this.displayErrorMessageBox(oCheckResult.isMissingAuthorization, oCheckResult.isError, sErrorMessage, sDetailMessage);
          }
        }
      }
    },

    _batchRequestCompletedHandler: function (oEvent) {
      var isCheckEmbeddedRequests = true;
      var oResponse = {};
      if (oEvent) {
        oResponse = oEvent.getParameter("response");
        if (oResponse) {
          var oCheckRequestResult = this.checkHttpStatus(oResponse.statusCode, oResponse.statusText, false);
          if (oCheckRequestResult.isMissingAuthorization || oCheckRequestResult.isError) {
            isCheckEmbeddedRequests = false;
          }
        }
      }
      if (isCheckEmbeddedRequests && oEvent) {
        var oCheckResultEmbeddedRequests = {};
        oCheckResultEmbeddedRequests = this.checkHttpStatusEmbeddedRequest(oEvent.getParameter("requests"));
        if (oCheckResultEmbeddedRequests.isMissingAuthorization || oCheckResultEmbeddedRequests.isError) {
          this.displayErrorMessageBox(oCheckResultEmbeddedRequests.isMissingAuthorization,
            oCheckResultEmbeddedRequests.isError,
            oCheckResultEmbeddedRequests.errorMessage.join(""),
            oCheckResultEmbeddedRequests.detailMessage);
        }
      }
    },
  };
});
