sap.ui.define([
  "com/sap/gtt/app/sample/pof/util/AsyncUtils",
], function (AsyncUtils) {
  "use strict";

  return {

    initPromises: function () {
      // We use jQuery Deferred object to resolve promise object externally
      var oPromiseDomRendered = AsyncUtils.deferredPromise();
      this.setPromise("promiseDomRendered", oPromiseDomRendered);
    },

    getPromise: function (sName) {
      var oPromise;

      var oModel = this.getModel("view");
      if (oModel) {
        var aPromises = oModel.getProperty("/promises");
        oPromise = aPromises[sName];
      }

      return oPromise;
    },

    setPromise: function (sName, oPromise) {
      var oModel = this.getModel("view");
      if (oModel) {
        var aPromises = oModel.getProperty("/promises");
        if (!aPromises) {
          aPromises = {};
          oModel.setProperty("/promises", aPromises);
        }

        aPromises[sName] = oPromise;
      }
    },
  };
});
