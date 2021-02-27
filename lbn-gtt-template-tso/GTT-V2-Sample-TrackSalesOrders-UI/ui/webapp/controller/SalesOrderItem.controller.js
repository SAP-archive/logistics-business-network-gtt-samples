sap.ui.define(
  [
    "./BaseDetailController",
    "sap/ui/model/json/JSONModel",
    "sap/ui/core/Fragment",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
  ],
  function (
    BaseDetailController,
    JSONModel,
    Fragment,
    Filter,
    FilterOperator,
    Sorter
  ) {
    "use strict";

    return BaseDetailController.extend("com.sap.gtt.app.sample.sof.controller.SalesOrderItem", {
      routeName: "salesOrderItem",

      initModel: function () {
        var model = new JSONModel({
          criticalThreshold: 604800000, // One week early/delay (in milliseconds)
        });
        this.setModel(model, this.routeName);
      },

      initControls: function () {
        var decimalCount = 3;
        var completionRate = Fragment.byId(this.createId("headerContentFragment"), "completionRate");
        this.initCompletionRate(completionRate, decimalCount);
        this.initGenericTags(["isDelayedGenericTag"]);
        this.registerEvents(this.routeName);
      },

      routePatternMatched: function (oEvent) {
        var model = this.getModel(this.routeName);
        var args = oEvent.getParameter("arguments");
        var id = args.id;
        var urlParams = args["?params"];
        model.setProperty("/urlParams", urlParams);

        // Bind the view with an entry
        var odataModel = this.getModel();
        odataModel.metadataLoaded().then(function () {
          var entitySetKey = odataModel.createKey("SalesOrderItem", {
            id: id,
          });
          this.bindView(entitySetKey);
        }.bind(this));
      },

      getExpandList: function () {
        return [
          "deliveryItems",
        ];
      },

      /**
       * update fulfillmentstatus, delivery items
       */
      updateView: function () {
        this.updateSalesOrderItemFulFillmentStatus();
        this.updateDeliveryItems();

        // refresh subsections
        this.refreshSubSection("milestoneView");
      },

      addCustomSorters: function (sorters) {
        if (sorters.length === 0) {
          sorters.push(new Sorter("deliveryNo", false));
          sorters.push(new Sorter("itemNo", false));
        }
        sorters.forEach(function (sorter) {
          if (sorter.sPath === "deliveryNo") {
            sorters.push(new Sorter("itemNo", sorter.bDescending));
          }
        });
      },

      updateDeliveryItems: function () {
        var smartTable = Fragment.byId(this.createId("deliveryItemsView"), "smartTable");
        smartTable.rebindTable();
      },

      updateSalesOrderItemFulFillmentStatus: function () {
        var bindingContext = this.getView().getBindingContext();
        var salesOrderItem = bindingContext.getObject({expand: "deliveryItems"});
        var deliveryItems = salesOrderItem.deliveryItems;
        var fulFillmentStatusSet = this.getFulfillmentStatusSet(deliveryItems);
        var model = this.getModel(this.routeName);
        model.setProperty("/deliveryItemsCount", deliveryItems.length);
        model.setProperty("/fulFillmentStatusSet", fulFillmentStatusSet);
      },


      // ======================================================================
      // Events
      // ======================================================================

      onDeliveryItemPressed: function (oEvent) {
        var source = oEvent.getSource();
        var bindingContext = source.getBindingContext();

        this.getRouter().navTo("deliveryItem", {
          id: bindingContext.getProperty("id"),
          params: {
            salesOrderId: this.getModel(this.routeName).getProperty("/urlParams/salesOrderId"),
          },
        });
      },
    });
  }
);
