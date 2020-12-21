sap.ui.define(["sap/ui/thirdparty/jquery"], function (jQuery) {
  "use strict";

  var AsyncUtils = {};

  /**
   * Get deferred object.
   *
   * @returns {Deferred} The deferred object
   */
  AsyncUtils.deferredPromise = function () {
    return jQuery.Deferred();
  };

  /**
   * Convert thenable object to Promise object
   *
   * @param {Object} thenable The thenable object, such as jQuery.Deferred
   * @returns {Promise} The Promise object
   */
  AsyncUtils.toPromise = function (thenable) {
    return Promise.resolve(thenable);
  };

  /**
   * Returns a Promise and calls the given handler, like
   * <code>Promise.prototype.finally</code>.
   *
   * @param {Promise} promise The Promise object
   * @param {function} fnOnFinally Callback function if this promise is settled
   * @returns {Promise} A new Promise object
   */
  AsyncUtils.finally = function (promise, fnOnFinally) {
    return promise.then(
      function (vResult) {
        return Promise.resolve(fnOnFinally()).then(function () {
          return vResult;
        });
      },
      function (vReason) {
        return Promise.resolve(fnOnFinally()).then(function () {
          throw vReason;
        });
      }
    );
  };

  return AsyncUtils;
});
