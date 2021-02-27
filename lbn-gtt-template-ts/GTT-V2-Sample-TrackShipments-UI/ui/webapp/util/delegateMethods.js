sap.ui.define([], function () {
  "use strict";

  /**
   * Delegate methods
   *
   * Please avoid using it unless you really need it.
   *
   * @param {object} delegate Delegation info
   * @param {*} delegate.from Delegation source
   * @param {*} delegate.to Delegation target
   * @param {string[]} delegate.methodNames Names of methods to be delegated
   */
  var delegateMethods = function (delegate) {
    delegate.methodNames.forEach(function (methodName) {
      if (typeof delegate.from[methodName] !== "function") {
        throw new TypeError("Method '" + methodName + "' doesn't exist in the context provided");
      }

      if (delegate.to[methodName] === undefined) {
        Object.defineProperty(delegate.to, methodName, {
          value: function () {
            return delegate.from[methodName].apply(delegate.from, arguments);
          },
          enumerable: true,
        });
      }
    }, delegate.to);
  };

  return delegateMethods;
});
