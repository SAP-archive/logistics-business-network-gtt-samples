sap.ui.define(["sap/base/assert"], function (assert) {
  "use strict";

  /**
   * An injectable i18n instance.
   *
   * @param {string} key I18n key
   * @param {string[]} [args] List of parameter values
   * @returns {string} Translated text
   *
   * @class
   *
   * @example
   * i18n.use(this.getModel("i18n"));
   * i18n("appTitle");  // "App Title"
   *
   * i18n.has("appTitle");  // true
   * i18n.has("notExist");  // false
   */
  var i18n = function (key, args) {
    assert(i18n._model !== undefined, "You should provide at least one ResourceModel in 'i18n.use()' before calling 'i18n()'.");

    try {
      return i18n._model.getResourceBundle().getText(key, args);
    } catch (_) {
      return key;
    }
  };

  /**
   * Register `ResourceModel` for this i18n instance
   *
   * @param {...sap.ui.model.resource.ResourceModel} model Resource model
   * @returns {i18n} The i18n instance
   *
   * @static
   *
   * @example
   * // Using single i18n model
   * i18n.use(model);
   *
   * // Using multiple i18n models
   * i18n.use(modelA);
   * i18n.use(modelB);  // Enhance modelA with modelB
   * // or
   * i18n.use(modelA, modelB);
   * // or
   * i18n.use(modelA).use(modelB);
   */
  i18n.use = function () {
    Array.from(arguments).forEach(function (model) {
      if (!i18n._model) {
        Object.defineProperty(i18n, "_model", {
          value: model,
        });
      } else {
        i18n._model.enhance(model.getResourceBundle());
      }
    });

    return i18n;
  };

  /**
   * Returns translated text for the given key, same as `i18n(key, args)`.
   *
   * @param {string} key I18n key
   * @param {string[]} [args] List of parameter values
   * @returns {string} Translated text
   *
   * @static
   */
  i18n.get = function (key, args) {
    return i18n(key, args);
  };

  /**
   * Checks whether a text for the given key can be found.
   *
   * @param {string} key I18n key
   * @returns {boolean} Whether a text for the given key can be found
   *
   * @static
   */
  i18n.has = function (key) {
    try {
      return (i18n._model.getResourceBundle().getText(key, [], true) !== undefined);
    } catch (_) {
      return false;
    }
  };

  return i18n;
});
