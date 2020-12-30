sap.ui.define([
], function () {
  "use strict";

  return {

    getText: function (sKey, aPlaceholderValues, sI18nModel)  {
      var sResult = "";
      // get the internationalized text
      if (this._oComponent) {
        var oResourceBundle = this._oComponent.getModel(sI18nModel || "i18n").getResourceBundle();
        sResult = oResourceBundle.getText(sKey, aPlaceholderValues);
      }
      return sResult;
    },

    setComponent: function (oComponent) {
      // set the component
      this._oComponent = oComponent;
    },
  };
});
