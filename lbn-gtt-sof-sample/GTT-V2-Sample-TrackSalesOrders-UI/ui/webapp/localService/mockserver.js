sap.ui.define(
  [
    "sap/ui/core/util/MockServer",
    "sap/ui/model/json/JSONModel",
    "sap/base/Log",
    "sap/base/util/UriParameters",
  ],
  function (MockServer, JSONModel, Log, UriParameters) {
    "use strict";

    var oMockServer;
    var _sAppModulePath = "com/sap/gtt/app/sample/sof/";

    var oMockServerInterface = {
      /**
       * Initializes the mock server asynchronously.
       * You can configure the delay with the URL parameter "serverDelay".
       * The local mock data in this folder is returned instead of the real data for testing.
       * @protected
       * @param {object} [oOptionsParameter] init parameters for the mockserver
       * @returns{Promise} a promise that is resolved when the mock server has been started
       */
      init: function (oOptionsParameter) {
        var oOptions = oOptionsParameter || {};

        return new Promise(function (fnResolve, fnReject) {
          var sManifestUrl = sap.ui.require.toUrl(_sAppModulePath + "manifest.json");
          var oManifestModel = new JSONModel(sManifestUrl);

          oManifestModel.attachRequestCompleted(function () {
            var oUriParameters = new UriParameters(window.location.href);

            // parse manifest for local metatadata URI
            var oMainDataSource = oManifestModel.getProperty("/sap.app/dataSources/mainService");
            var sMockServerUrl = sap.ui.require.toUrl(_sAppModulePath + oMainDataSource.uri); // "/sof/odata/v1/"

            // ensure there is a trailing slash
            sMockServerUrl = /.*\/$/.test(sMockServerUrl) ? sMockServerUrl : sMockServerUrl + "/";

            // create a mock server instance or stop the existing one to reinitialize
            if (!oMockServer) {
              oMockServer = new MockServer({
                rootUri: sMockServerUrl.replace("/base/", ""),
              });
            } else {
              oMockServer.stop();
            }

            // configure mock server with the given options or a default delay of 0.5s
            MockServer.config({
              autoRespond: true,
              autoRespondAfter: oOptions.delay || oUriParameters.get("serverDelay") || 500,
            });

            // load local mock data
            var metadataLocalUri = oMainDataSource.settings.localUri;
            var sMetadataUrl = sap.ui.require.toUrl(_sAppModulePath + metadataLocalUri);

            var mockdataLocalUri = metadataLocalUri.replace("metadata.xml", "mockdata");
            var sJsonFilesUrl = sap.ui.require.toUrl(_sAppModulePath + mockdataLocalUri);

            // simulate all requests using mock data
            oMockServer.simulate(sMetadataUrl, {
              sMockdataBaseUrl: sJsonFilesUrl,
              bGenerateMissingMockData: true,
            });

            var aRequests = oMockServer.getRequests();

            // compose an error response for each request
            var fnResponse = function (iErrCode, sMessage, aRequest) {
              aRequest.response = function (oXhr) {
                oXhr.respond(
                  iErrCode,
                  {
                    "Content-Type": "text/plain;charset=utf-8",
                  },
                  sMessage
                );
              };
            };

            // simulate metadata errors
            if (oOptions.metadataError || oUriParameters.get("metadataError")) {
              aRequests.forEach(function (aEntry) {
                if (aEntry.path.toString().indexOf("$metadata") > -1) {
                  fnResponse(500, "metadata Error", aEntry);
                }
              });
            }

            // simulate request errors
            var sErrorParam = oOptions.errorType || oUriParameters.get("errorType");
            var iErrorCode = sErrorParam === "badRequest" ? 400 : 500;
            if (sErrorParam) {
              aRequests.forEach(function (aEntry) {
                fnResponse(iErrorCode, sErrorParam, aEntry);
              });
            }

            // custom mock behaviour may be added here

            // set requests and start the server
            oMockServer.setRequests(aRequests);
            oMockServer.start();

            // rest service
            var oJsonDataSource = oManifestModel.getProperty("/sap.app/dataSources/restService");
            var jsonMockServerUrl = sap.ui.require.toUrl(_sAppModulePath + oJsonDataSource.uri);
            var jsonMockServer = new MockServer({
              rootUri: jsonMockServerUrl,
              requests: [
                {
                  method: "GET",
                  path: /\/fulfillmentProcessFlow(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "fulfillmentProcessFlow.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/documentFlow(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "documentFlow.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/executionFlow(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "executionFlow.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/eventReportHistory(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "historicalEvents.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/plannedRoute(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "plannedRoutes.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/actualRoute(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "actualRoutes.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/routes(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "routes.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/sideContent(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "sideContent.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/fulfillmentStatus(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "fulfillmentStatus.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/carrierRefDocuments(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "carrierRefDocuments.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/impactAnalysis\/initialNodes(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "impactAnalysis/initialNodes.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/impactAnalysis\/nextNodes(.*)/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "impactAnalysis/nextNodes.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: "/hereKey",
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "hereKey.json"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
              ],
            });

            jsonMockServer.start();

            // i18n and annotation service
            var jsonMockServer2 = new MockServer({
              rootUri: jsonMockServerUrl.replace("/base/", ""),
              requests: [
                {
                  method: "HEAD",
                  path: /token.json(?:\?.+)?/, // "token.json?_=1234567"
                  response: function (oXhr) {
                    oXhr.respond(200, {
                      "X-CSRF-Token": "csrf_token_" + Date.now(),
                    });
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/uiAnnotation(.*)/,
                  response: function (oXhr) {
                    var localUri = oManifestModel.getProperty("/sap.app/dataSources/mainAnnotations/settings/localUri");
                    var mockdataFileUrl = sap.ui.require.toUrl(_sAppModulePath + localUri);
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
                {
                  method: "GET",
                  path: /\/i18n\/i18n.properties/,
                  response: function (oXhr) {
                    var mockdataFileUrl = sap.ui.require.toUrl(
                      _sAppModulePath + oJsonDataSource.settings.localUri + "@i18n.properties"
                    );
                    oXhr.respondFile(200, {}, mockdataFileUrl);
                    return true;
                  },
                },
              ],
            });
            jsonMockServer2.start();

            Log.info("Running the app with mock data");
            fnResolve();
          });

          oManifestModel.attachRequestFailed(function () {
            var sError = "Failed to load application manifest";

            Log.error(sError);
            fnReject(new Error(sError));
          });
        });
      },
    };

    return oMockServerInterface;
  }
);
