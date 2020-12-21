sap.ui.define([
  "sap/ui/model/json/JSONModel",
], function (JSONModel) {
  "use strict";

  return {

    /**
     * Convenience method for getting the view model by name.
     * @public
     * @param {string} sName the model name
     * @returns {sap.ui.model.Model} the model instance
     */
    getModel: function (sName) {
      return this.getView().getModel(sName);
    },

    /**
     * Convenience method for setting the view model.
     * @public
     * @param {sap.ui.model.Model} oModel the model instance
     * @param {string} sName the model name
     * @returns {sap.ui.mvc.View} the view instance
     */
    setModel: function (oModel, sName) {
      return this.getView().setModel(oModel, sName);
    },

    initViewModel: function () {
      var oModel = new JSONModel();
      this.setModel(oModel, "view");
    },

    getViewModel: function () {
      return this.getView().getModel("view");
    },

    /**
     * @abstract
     */
    initModel: function () {
    },
  };
});
