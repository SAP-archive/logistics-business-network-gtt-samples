/* eslint-disable guard-for-in */
sap.ui.define(
  [
    "./BaseController",
  ], function (
    BaseController
  ) {
    "use strict";

    var controllerClassInfo = {
      onLinkPressed: function () {
        this.getRouter().navTo("salesOrderList");
      },
    };

    return BaseController.extend(
      "com.sap.gtt.app.sample.sof.controller.NotFound",
      controllerClassInfo
    );
  }
);
