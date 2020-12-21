sap.ui.define(
  [
    "com/sap/gtt/app/sample/pof/controller/BaseController",
  ], function (
    BaseController
  ) {
    "use strict";

    var controllerClassInfo = {
      onLinkPressed: function () {
        this.getRouter().navTo("purchaseOrderList");
      },
    };

    return BaseController.extend(
      "com.sap.gtt.app.sample.pof.controller.NotFound",
      controllerClassInfo
    );
  }
);
