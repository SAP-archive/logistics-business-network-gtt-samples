sap.ui.define([
], function () {
  "use strict";

  return {
    delegateComponentMethods: function (oClassInfo) {
      var aMethodNames = ["getRouter", "getEventBus"];
      aMethodNames.forEach(function (sMethodName) {
        oClassInfo[sMethodName] = function () {
          var oComponent = this.getOwnerComponent();
          return oComponent[sMethodName].apply(oComponent, arguments);
        };
      });

      return oClassInfo;
    },

    /**
     * Get startup parameters of component
     *
     * @returns {Object} The startup parameters
     */
    getStartupParameters: function () {
      var oStartupParameters = {};
      var oComponent = this.getOwnerComponent();
      var oComponentData = oComponent.getComponentData();

      if (oComponentData) {
        oStartupParameters = oComponentData.oStartupParameters;
      }

      return oStartupParameters;
    },

    /**
     * Getter for the resource bundle.
     * @public
     * @param {string} sName The name of i18n model
     * @returns {sap.ui.model.resource.ResourceModel} the resourceModel of the component
     */
    getResourceBundle: function (sName) {
      var sI18nModelName = sName || "i18n";
      var oComponent = this.getOwnerComponent();
      return oComponent.getModel(sI18nModelName).getResourceBundle();
    },

    /**
     * Get component config
     * @returns {Object} The component config object
     */
    getComponentConfig: function () {
      var oComponent = this.getOwnerComponent();
      return oComponent.getManifestEntry("/sap.ui5/config");
    },
  };
});
