sap.ui.define([], function () {
  "use strict";

  /**
   * Create a new Api instance
   *
   * This constructor is for internal use, it is not recommended to
   * create a new instance in your code, please use `Api.define()` instead.
   *
   * @param {string} dataSource Name of dataSource
   *
   * @class
   *
   * @example
   * onInit: () => {
   *   this.myApi = new Api("myDataSource");
   * },
   * fetchInfo: id => HttpClient.get(
   *   this.myApi.createUrl(`/info/${id}`)
   * ),
   */
  var Api = function (dataSource) {
    if (!this._componentPath || !this._dataSources) {
      throw new Error("The Api has not been initialized, please call 'Api.initialize(component)' first.");
    }

    var target = this._dataSources[dataSource];
    if (!target) {
      throw new Error("Data source '" + dataSource + "' does not exist, please check the manifest.json");
    }

    this._baseUrl = sap.ui.require.toUrl([this._componentPath, target.uri].join("/"));
  };

  /**
   * Initialize the Api with component
   *
   * @param {sap.ui.core.Component} component Component
   * @static
   */
  Api.initialize = function (component) {
    Object.defineProperties(Api.prototype, {
      "_componentPath": {
        value: component.getManifestObject().getComponentName().replace(/\./g, "/"),
        writable: true,
      },
      "_dataSources": {
        value: component.getManifestEntry("/sap.app/dataSources"),
        writable: true,
      },
    });
  };

  /**
   * Define a new API collection
   *
   * @param {string} dataSource Name of dataSource
   * @param {object} apiInfo API info
   * @returns {object} API collection with `createUrl(path)`
   * @static
   *
   * @example
   * const myApi = Api.define("myDataSource", {
   *   // ...
   *   getInfo: id => HttpClient.get(
   *     this.createUrl(`/info/${id}`)
   *   ),
   * });
   *
   * // Usage
   * myApi.getInfo("bob");
   */
  Api.define = function (dataSource, apiInfo) {
    Object.defineProperty(apiInfo, "createUrl", {
      value: function (path) {
        if (!(this._api instanceof Api)) {
          this._api = new Api(dataSource);
        }
        return this._api.createUrl(path);
      },
      enumerable: true,
    });

    return apiInfo;
  };

  /**
   * Create the URL with relative path
   *
   * @param {string} path Relative path
   * @returns {string} URL
   * @final
   */
  Api.prototype.createUrl = function (path) {
    return this._baseUrl + path;
  };

  return Api;
});
