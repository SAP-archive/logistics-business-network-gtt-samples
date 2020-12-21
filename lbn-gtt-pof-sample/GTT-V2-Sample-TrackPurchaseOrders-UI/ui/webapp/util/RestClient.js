sap.ui.define([
  "sap/ui/thirdparty/jquery",
  "sap/base/Log",
  "sap/base/security/encodeURLParameters",
  "./AsyncUtils",
],
function (jQuery, Log, encodeURLParameters, AsyncUtils) {
  "use strict";

  var RestClient = {};

  var XSRF_HEADER_NAME = "X-CSRF-Token";

  var RequestType = {
    XHR: "xhr",
    Fetch: "fetch",
  };

  var _requestType = RequestType.XHR;
  var _csrfToken;

  function _handleRequestHeaders(request) {
    if (!request.headers) {
      request.headers = {};
    }

    Object.assign(request.headers, {
      "Accept-Language": sap.ui.getCore().getConfiguration().getLanguage(),
    });
  }

  function _handleResponseHeaders(responseHeadersString) {
    var rHeaders = /^(.*?):[ \t]*([^\r\n]*)$/gm;
    var responseHeaders = {};
    var match;

    while ((match = rHeaders.exec(responseHeadersString))) {
      var key = match[1].toLowerCase();
      responseHeaders[key] = match[2];
    }

    return responseHeaders;
  }

  RestClient.setRequestType = function (type) {
    _requestType = type;
  };

  RestClient.getRequestType = function () {
    return _requestType;
  };

  RestClient.getCSRFToken = function () {
    if (_csrfToken) {
      return Promise.resolve(_csrfToken);
    }

    return this.fetchCSRFToken().then(function (newToken) {
      _csrfToken = newToken;

      return _csrfToken;
    });
  };

  RestClient.fetchCSRFToken = function () {
    var xsrfHeaderName = XSRF_HEADER_NAME;

    var options = {
      headers: {},
    };

    options.headers[xsrfHeaderName] = "fetch";

    return this.head("/token.json", options).then(function (res) {
      var token = res.headers[xsrfHeaderName.toLowerCase()];

      if (!token) {
        throw new Error("Failed to fetch CSRF token");
      }

      return token;
    });
  };

  RestClient.get = function (url, options) {
    var config = Object.assign({}, options, {
      method: "GET",
    });

    return this.request(url, config).then(function (res) {
      return res.data;
    });
  };

  RestClient.post = function (url, data, options) {
    var config = Object.assign({}, options, {
      method: "POST",
      data: data,
      xsrfHeaderName: XSRF_HEADER_NAME,
    });

    return this.request(url, config).then(function (res) {
      return res.data;
    });
  };

  RestClient.put = function (url, data, options) {
    var config = Object.assign({}, options, {
      method: "PUT",
      data: data,
      xsrfHeaderName: XSRF_HEADER_NAME,
    });

    return this.request(url, config).then(function (res) {
      return res.data;
    });
  };

  RestClient.delete = function (url, options) {
    var config = Object.assign({}, options, {
      method: "DELETE",
      xsrfHeaderName: XSRF_HEADER_NAME,
    });

    return this.request(url, config).then(function (res) {
      return res.data;
    });
  };

  RestClient.head = function (url, options) {
    var config = Object.assign({}, options, {
      method: "HEAD",
    });

    return this.request(url, config);
  };

  RestClient.request = function (url, config) {
    var requestType = this.getRequestType();
    var requestFunc;

    if (requestType === RequestType.Fetch) {
      requestFunc = this._fetch.bind(this);
    } else {
      requestFunc = this._jqXhr.bind(this);
    }

    var oConfig = config || {};
    _handleRequestHeaders(oConfig);

    return requestFunc(url, oConfig);
  };

  /* =========================================================== */
  /* internal methods for jQuery XHR                             */
  /* =========================================================== */

  function _parseJqXhrResponse(jqXHR) {
    var response = {};
    response.data = jqXHR.responseJSON || jqXHR.responseText;
    response.status = jqXHR.status;
    response.statusText = jqXHR.statusText;
    response.headers = _handleResponseHeaders(jqXHR.getAllResponseHeaders());

    return response;
  }

  RestClient._jqXhr = function (url, config) {
    var settings = {},
      sUrl = url;

    var csrfTokenPromise = config.xsrfHeaderName ? this.getCSRFToken() : Promise.resolve();

    return csrfTokenPromise.then(function (csrfToken) {
      if (config.method) {
        settings.method = config.method;
      }

      settings.headers = {};

      if (config.headers) {
        Object.assign(settings.headers, config.headers);
      }

      if (config.xsrfHeaderName) {
        settings.headers[config.xsrfHeaderName] = csrfToken;
      }

      if (config.params) {
        sUrl += "?" + encodeURLParameters(config.params);
      }

      if (config.data) {
        settings.contentType = "application/json";
        settings.data = JSON.stringify(config.data);
      }

      var jqPromise = jQuery.ajax(sUrl, settings).then(
        function (data, textStatus, jqXHR) {
          var response = _parseJqXhrResponse(jqXHR);
          response.data = data;

          return response;
        },
        function (jqXHR, textStatus, errorThrown) {
          // 404, 500
          var error = {};
          error.response = _parseJqXhrResponse(jqXHR);

          return error;
        }
      );

      return AsyncUtils.toPromise(jqPromise);
    });
  };

  /* =========================================================== */
  /* internal methods for fetch                                  */
  /* =========================================================== */

  RestClient._fetch = function (url, config) {
    Log.warning("unimplemented");
  };

  return RestClient;
});
