sap.ui.define(
  [
    "./Shipment.controller",
  ],
  function (
    ShipmentController
  ) {
    "use strict";

    return ShipmentController.extend("com.sap.gtt.app.sample.sst.controller.FreightUnit", {
      routeName: "freightUnit",

      onBeforeRendering: function () {
        this.getModel("view").setProperty("/isFreightUnit", true);
      },

      getEntitySetKey: function (odataModel, id) {
        return odataModel.createKey("FreightUnit", {
          id: id,
        });
      },

      getExpandList: function () {
        return [
          "departureLocation",
          "arrivalLocation",
          "shippingType/localized",
          "incoterms/localized",
          "processStatus/localized",
          "executionStatus/localized",
          "freightUnitItems",
        ];
      },

      updateView: function () {
        ShipmentController.prototype.updateView.apply(this, arguments);
        this.updateMaterialInformation();
      },

      updateMaterialInformation: function () {
        var urlParams = this.getModel("view").getProperty("/urlParams");
        var context = this.getView().getBindingContext();
        var freightUnit = context.getObject({expand: "freightUnitItems"});
        var matchedItems = freightUnit.freightUnitItems.filter(function (item) {
          return item.itemNo === urlParams.itemNo;
        });
        this.getModel("view").setProperty("/freightUnitItem", matchedItems[0]);
      },
    });
  }
);
