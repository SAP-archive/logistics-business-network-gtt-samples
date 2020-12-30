sap.ui.define([
  "sap/ui/model/json/JSONModel",
  "sap/base/strings/formatMessage",
], function (
  JSONModel,
  formatMessage
) {
  "use strict";

  var appModulePath = "com/sap/gtt/app/sample/sst/";
  var manifestModel = new JSONModel();
  var ServiceUtils = {};

  ServiceUtils.init = function () {
    var manifestUrl = sap.ui.require.toUrl(appModulePath + "manifest.json");
    manifestModel.loadData(manifestUrl, null, false);
  };

  ServiceUtils.getDataSource = function (name) {
    return manifestModel.getProperty(formatMessage("/sap.app/dataSources/{0}", name));
  };

  ServiceUtils.getUrl = function (path) {
    return sap.ui.require.toUrl(appModulePath + path);
  };

  return ServiceUtils;
});
