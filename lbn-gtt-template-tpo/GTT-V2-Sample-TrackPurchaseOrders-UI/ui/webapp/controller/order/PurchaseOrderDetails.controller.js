sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "sap/ui/model/json/JSONModel",
  "sap/ui/core/Fragment",
], function (BaseController, Constants, JSONModel, Fragment) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.order.PurchaseOrderDetails", {
    routeName: "PurchaseOrderDetails",

    initModel: function () {
      var oModel = new JSONModel({});
      this.setModel(oModel, this.routeName);
    },

    routePatternMatched: function (oEvent) {
      var oModel = this.getModel(this.routeName),
        oOdataModel = this.getModel(),
        oArgs = oEvent.getParameter("arguments"),
        sPOId = oArgs.id,
        oUrlParams = oArgs["?params"];

      oModel.setProperty(Constants.PURCHASE_ORDER_ID_PATH, sPOId);
      oModel.setProperty("/urlParams", oUrlParams);

      oOdataModel.metadataLoaded()
        .then(function () {
          var sEntitySetKey = oOdataModel.createKey("PurchaseOrder", {
            id: sPOId,
          });
          this.bindView(sEntitySetKey,{
            expand: "toSupplierLocation,toReceivingLocation,purchaseOrderItemTPs/incoterms,purchaseOrderItemTPs/receivingLocationType,receivingLocationType,purchaseOrderItemTPs/supplierLocationType,purchaseOrderItemTPs/toSupplierLocation,purchaseOrderItemTPs/toReceivingLocation,supplierLocationType",
          });
        }.bind(this));
    },

    updateView: function () {
      var oBindingContext = this.getView().getBindingContext(),
        sPurchaseOrderNo = oBindingContext.getProperty("id"),
        oDocumentFlowController = this.byId("documentFlowView").getController();

      this.getModel(this.routeName).setProperty(Constants.PURCHASE_ORDER_ID_PATH, sPurchaseOrderNo);

      oDocumentFlowController.setFocusGroup(Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER);
      oDocumentFlowController.refresh();
    },

    initControls: function () {
      var oHeaderCompletionRate = this.byId("headerCompletionRate");
      var oItemCompletionRate = Fragment.byId(this.createId("purchaseOrderDetailsItemsTable"), "itemCompletionRate");
      this.initCompletionRate([oHeaderCompletionRate, oItemCompletionRate], Constants.TWO_DECIMALS_AFTER_DOT);
    },

    // ======================================================================
    // Events
    // ======================================================================

    onBeforeRebindTable: function (oEvent) {
      var oBindingParams = oEvent.getParameter("bindingParams");
      this.addDefaultSorters(oBindingParams.sorter, ["itemNo"]);
    },

    /**
     * Route to the PO Item details page
     * @param {*} oEvent press event
     */
    onPurchaseOrderDetailsItemPressed: function (oEvent) {
      var oContext = oEvent.getSource().getBindingContext();
      var sPath = oContext.getPath();
      var oModel = oContext.getModel();
      var sId = oModel.getProperty(sPath + "/id");

      this.getRouter().navTo("PurchaseOrderItemDetails", {
        id: sId,
      });
    },
  });
});
