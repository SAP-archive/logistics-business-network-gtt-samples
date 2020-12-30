sap.ui.define(["sap/base/strings/formatMessage"], function (formatMessage) {
  "use strict";

  var ServiceUtils = {};

  var component;

  ServiceUtils.init = function (options) {
    component = options.component;
  };

  ServiceUtils.getDataSource = function (name) {
    return component.getManifestEntry(formatMessage("/sap.app/dataSources/{0}", name));
  };

  ServiceUtils.getUrl = function (path) {
    var manifestObject = component.getManifestObject();
    var componentName = manifestObject.getComponentName();
    return sap.ui.require.toUrl([componentName.replace(/\./g, "/"), path].join("/"));
  };

  return ServiceUtils;
});
