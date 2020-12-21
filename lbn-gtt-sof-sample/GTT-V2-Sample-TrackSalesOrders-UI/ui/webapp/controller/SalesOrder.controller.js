sap.ui.define(
  [
    "./BaseDetailController",
    "sap/ui/core/Fragment",
    "sap/ui/model/Sorter",
  ],
  function (
    BaseDetailController,
    Fragment,
    Sorter
  ) {
    "use strict";

    return BaseDetailController.extend("com.sap.gtt.app.sample.sof.controller.SalesOrder", {
      routeName: "salesOrder",

      initControls: function () {
        var completionRate = Fragment.byId(this.createId("headerContentFragment"), "completionRate");
        this.initCompletionRate(completionRate);
        this.initGenericTags(["isDelayedGenericTag"]);
        this.registerEvents(this.routeName);
        this.initHarveryBall();
      },

      initHarveryBall: function () {
        Fragment.byId(this.createId("salesOrderItemsView"), "harveyBallChart").addEventDelegate({
          onAfterRendering: function (oEvent) {
            var source = oEvent.srcControl;
            var valueText = source.$().find(".sapSuiteHBMCValue").text();
            source.$().find(".sapSuiteHBMCValue").text(this.formatter.floatNumberFormat.format(valueText));
          }.bind(this),
        });
      },

      routePatternMatched: function (oEvent) {
        var model = this.getModel(this.routeName);

        var args = oEvent.getParameter("arguments");
        var id = args.id;
        model.setProperty("/salesOrderId", id);

        // Bind the view with an entry
        var odataModel = this.getModel();
        odataModel.metadataLoaded().then(function () {
          var entitySetKey = odataModel.createKey("SalesOrder", {
            id: id,
          });
          this.bindView(entitySetKey);
        }.bind(this));
      },

      getExpandList: function () {
        return [
          "incoterms",
          "shipToPartyLocation",
        ];
      },

      /**
       * update sales order items and document flow
       */
      updateView: function () {
        // refresh subsections
        this.refreshSubSection("documentFlowView");
      },

      onBeforeRebindSalesOrderItemsTable: function (oEvent) {
        var params = oEvent.getParameter("bindingParams");
        this.addCustomSorters(params.sorter);
      },

      addCustomSorters: function (sorters) {
        if (sorters.length === 0) {
          sorters.push(new Sorter("salesOrderItem/itemNo", false));
        }
      },


      // ======================================================================
      // Events
      // ======================================================================

      onSalesOrderItemPressed: function (oEvent) {
        var source = oEvent.getSource();
        var bindingContext = source.getBindingContext();

        this.getRouter().navTo("salesOrderItem", {
          id: bindingContext.getProperty("salesOrderItem/id"),
          params: {
            salesOrderId: this.getModel(this.routeName).getProperty("/salesOrderId"),
          },
        });
      },
    });
  }
);
