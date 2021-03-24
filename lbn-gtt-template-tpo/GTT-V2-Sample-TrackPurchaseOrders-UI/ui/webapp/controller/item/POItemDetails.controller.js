sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/util/RestClient",
  "sap/ui/model/json/JSONModel",
],
function (BaseController, Constants, RestClient, JSONModel) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.item.POItemDetails", {
    routeName: "PurchaseOrderItemDetails",

    initModel: function () {
      var oModel = new JSONModel({
        // One week in milliseconds
        criticalThreshold: 604800000,
      });
      this.setModel(oModel, this.routeName);
    },

    initControls: function () {
      var oCompletionRate = this.byId("headerCompletionRate");
      this.initCompletionRate([oCompletionRate]);
    },

    routePatternMatched: function (oEvent) {
      var oModel = this.getModel(this.routeName),
        oODataModel = this.getModel(),
        oArgs = oEvent.getParameter("arguments"),
        sPOItemId = oArgs.id,
        oUrlParams = oArgs["?params"];

      oModel.setProperty("/purchaseOrderItemId", sPOItemId);
      oModel.setProperty("/urlParams", oUrlParams);

      oODataModel.metadataLoaded()
        .then(function () {
          var sEntitySetKey = oODataModel.createKey("PurchaseOrderItem", {
            id: sPOItemId,
          });
          this.bindView(sEntitySetKey, {
            expand: "receivingLocationType,supplierLocationType,scheduleLines,incoterms,purchaseOrder,toSupplierLocation,toReceivingLocation,inboundDeliveryItems/incoterms,inboundDeliveryItems/supplierLocationType,inboundDeliveryItems/plantLocationType,inboundDeliveryItems/toSupplierLocation,inboundDeliveryItems/toPlantLocation",
          });
        }.bind(this));
    },

    updateView: function () {
      var oBindingContext = this.getView().getBindingContext();
      var sPOItemNo = oBindingContext.getProperty("id");

      this.getModel(this.routeName).setProperty("/purchaseOrderItemId", sPOItemNo);

      this._refreshProcessFulfillment();
      this._refreshDocumentFlow();
    },

    /**
     * Fire request to refresh milestones fulfillment process data.
     */
    _refreshProcessFulfillment: function () {
      var oMilestoneProcessController = this.byId("milestoneProcessView").getController();
      oMilestoneProcessController.refresh();
    },

    /**
     * Fire request to refresh document flow process data.
     */
    _refreshDocumentFlow: function () {
      var oDocumentFlowController = this.byId("documentFlowView").getController();
      oDocumentFlowController.setFocusGroup(Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM);
      oDocumentFlowController.refresh();
    },

    // ======================================================================
    // Events
    // ======================================================================

    onBeforeRebindTable: function (oEvent) {
      var oBindingParams = oEvent.getParameter("bindingParams");
      this.addDefaultSorters(oBindingParams.sorter, ["inboundDeliveryNo", "itemNo"]);
    },

    /**
     * Route to the Delivery Item details page
     * @param {Object} oEvent press event
     */
    onDeliveryItemPressed: function (oEvent) {
      var oContext = oEvent.getSource().getBindingContext();
      var sPath = oContext.getPath();
      var oModel = oContext.getModel();
      var sId = oModel.getProperty(sPath + "/id");

      this.getRouter().navTo("DeliveryItemDetails", {
        id: sId,
      });
    },

  });
});
