sap.ui.define([
  "sap/base/strings/formatMessage",
], function (formatMessage) {
  "use strict";

  var ServiceUtils = {};
  var oComponent;

  ServiceUtils.init = function (options) {
    oComponent = options.component;
  };

  ServiceUtils.getDataSource = function (sName) {
    return oComponent.getManifestEntry(formatMessage("/sap.app/dataSources/{0}", sName));
  };

  ServiceUtils.getUrl = function (path) {
    var oManifest = oComponent.getManifestObject();
    var sComponentName = oManifest.getComponentName();
    return sap.ui.require.toUrl([sComponentName.replace(/\./g, "/"), path].join("/"));
  };

  return ServiceUtils;
});
