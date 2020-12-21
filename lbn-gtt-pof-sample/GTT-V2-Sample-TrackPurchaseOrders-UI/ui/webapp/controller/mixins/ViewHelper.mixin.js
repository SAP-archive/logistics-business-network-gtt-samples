sap.ui.define([
], function () {
  "use strict";

  return {

    /**
     * Binding the view and free the busy state
     * @param {string} sEntitySetKey The key of the entityset like EntitySet('id')
     * @param {object} oParameters parameters object
     */
    bindView: function (sEntitySetKey, oParameters) {
      var oView = this.getView();

      oView.bindElement({
        path: "/" + sEntitySetKey,
        parameters: oParameters,
        events: {
          change: this.onBindingChange.bind(this),
          dataRequested: this.onDataRequested.bind(this),
          dataReceived: this.onDataReceived.bind(this),
        },
      });
    },

    /**
     * @abstract
     */
    updateView: function () {
    },

    /**
     * Add loading indicator for the view - busy state
     * @param {sap.ui.model.json.JSONModel} oModel The json model
     */
    setViewBusy: function (oModel) {
      var iOriginalBusyDelay = this.getView().getBusyIndicatorDelay();
      oModel.setProperty("/busy", true);
      oModel.setProperty("/delay", iOriginalBusyDelay);
    },

    /**
     * Remove loading indicator for the view - not busy state
     * @param {sap.ui.model.json.JSONModel} oModel The json model
     */
    setViewFree: function (oModel) {
      var iOriginalBusyDelay = this.getView().getBusyIndicatorDelay();
      oModel.setProperty("/busy", false);
      oModel.setProperty("/delay", iOriginalBusyDelay);
    },
  };
});
