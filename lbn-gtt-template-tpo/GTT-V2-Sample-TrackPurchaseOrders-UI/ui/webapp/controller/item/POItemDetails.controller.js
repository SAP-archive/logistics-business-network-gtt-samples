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
      var oModel = new JSONModel({});
      this.setModel(oModel, this.routeName);
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
            expand: "receivingLocationType,supplierLocationType,scheduleLines,incoterms,purchaseOrder,toSupplierLocation,toReceivingLocation,inboundDeliveryItems/inboundDeliveryItem/incoterms,inboundDeliveryItems/inboundDeliveryItem/supplierLocationType,inboundDeliveryItems/inboundDeliveryItem/plantLocationType,inboundDeliveryItems/inboundDeliveryItem/toSupplierLocation,inboundDeliveryItems/inboundDeliveryItem/toPlantLocation",
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

    _refreshProcessFulfillment: function () {
      var oMilestoneProcessController = this.byId("milestoneProcessView").getController();
      oMilestoneProcessController.refresh();
    },

    _getProcessFulfillmentRequest: function () {
      return RestClient.request();
    },

    _refreshDocumentFlow: function () {
      var oDocumentFlowController = this.byId("documentFlowView").getController();
      oDocumentFlowController.setFocusGroup(Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM);
      oDocumentFlowController.refresh();
    },

    initControls: function () {
      var oCompletionRate = this.byId("headerCompletionRate");
      this.initCompletionRate([oCompletionRate]);
    },
    // ======================================================================
    // Events
    // ======================================================================

    onBeforeRebindTable: function (oEvent) {
      var oBindingParams = oEvent.getParameter("bindingParams");
      this.addDefaultSorters(oBindingParams.sorter, ["inboundDeliveryItem/inboundDeliveryNo", "lineNo"]);
    },

    /**
     * Route to the Delivery Item details page
     * @param {Object} oEvent press event
     */
    onDeliveryItemPressed: function (oEvent) {
      var oContext = oEvent.getSource().getBindingContext();
      var sPath = oContext.getPath();
      var oModel = oContext.getModel();
      var sId = oModel.getProperty(sPath + "/inboundDeliveryItem/id");

      this.getRouter().navTo("DeliveryItemDetails", {
        id: sId,
      });
    },

  });
});
