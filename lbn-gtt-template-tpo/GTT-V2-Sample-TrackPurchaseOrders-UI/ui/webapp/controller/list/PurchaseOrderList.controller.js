sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "com/sap/gtt/app/sample/pof/util/Constants",
], function (BaseController, JSONModel, Constants) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.list.PurchaseOrderList", {
    routeName: "purchaseOrderList",

    onInit: function () {
      this._oPOSmartTable 		  = this.byId("purchaseOrderSmartTable");
      this._oPOInnerTable 		  = this.byId("innerTable");
      this._oPOItemsSmartTable 	= this.byId("purchaseOrderItemsSmartTable");
      this._oPOItemsInnerTable 	= this.byId("innerTableItem");

      BaseController.prototype.onInit.apply(this, arguments);
    },

    initModel: function () {
      var oListModel = new JSONModel({customFilters: {}});
      this.setModel(oListModel, this.routeName);
    },

    initViewModel: function () {
      BaseController.prototype.initViewModel.call(this);

      this.getViewModel().setData({
        selectedTabFitlerKey: "purchaseOrderTab",
        purchaseOrderAmount: "0",
        purchaseOrderItemsAmount: "0"});
    },

    subscribeEvents: function () {
      this._oPOSmartTable.attachDataReceived(this._purchaseOrderDataReceived.bind(this));
      this._oPOItemsSmartTable.attachDataReceived(this._purchaseOrderItemsDataReceived.bind(this));
    },

    unsubscribeEvents: function () {
      this._oPOSmartTable.detachDataReceived(this._purchaseOrderDataReceived.bind(this));
      this._oPOItemsSmartTable.detachDataReceived(this._purchaseOrderItemsDataReceived.bind(this));
    },

    // ======================================================================
    // Private methods.
    // ======================================================================

    _purchaseOrderDataReceived: function (oData) {
      var oDataParam = oData.getParameters().getParameter("data"),
        iItemsAmount = 0;
      if(oDataParam) {
        iItemsAmount = oDataParam.__count ? oDataParam.__count : this._oPOInnerTable.getMaxItemsCount();
      }
      this.getViewModel().setProperty("/purchaseOrderAmount", iItemsAmount);
    },

    _purchaseOrderItemsDataReceived: function (oData) {
      var oDataParam = oData.getParameters().getParameter("data"),
        iItemsAmount = 0;
      if(oDataParam) {
        iItemsAmount = oDataParam.__count ? oDataParam.__count : this._oPOItemsInnerTable.getMaxItemsCount();
      }
      this.getViewModel().setProperty("/purchaseOrderItemsAmount", iItemsAmount);
    },

    /**
     * Change the filter path if items should be filtered by child nodes.
     * @param {object} oBindingParams binding parameters
     */
    _changeFilterBeforeRequest: function (oBindingParams) {
      var aFiltersParam = oBindingParams.filters;
      if(!aFiltersParam.length) {
        return;
      }
      var oMainFiter = aFiltersParam[0];

      this._setFilterPath(oMainFiter);
    },

    /**
     * Recursive function to change the filter path.
     * @param {sap.ui.model.Filter} oFilter filter object
     */
    _setFilterPath: function (oFilter) {
      var aFilters = oFilter.aFilters;
      if (aFilters) {
        for (var iInd = 0; iInd < aFilters.length; iInd++) {
          var oInnerFilter = aFilters[iInd];
          this._setFilterPath(oInnerFilter);
        }
      } else if (oFilter.sPath) {
        if(oFilter.sPath === Constants.PO_ITEM_MATERIAL_ID_PROP || oFilter.sPath === Constants.PO_ITEM_MATERIAL_DESC_PROP) {
          oFilter.sPath = Constants.PO_TO_PO_ITEM_NAV_PATH + "/" + oFilter.sPath;
        }
      }
    },

    initControls: function () {
      var oCompletionRate = this.byId("orderCompletionRate");
      var oItemCompletionRate = this.byId("itemCompletionRate");
      this.initCompletionRate([oCompletionRate,oItemCompletionRate]);
    },

    // ======================================================================
    // Events
    // ======================================================================

    /**
     * PO SmartTable lifecycle 'beforeRebind' hook event.
     * Add parameters before request.
     * @param {sap.ui.base.Event} oEvent event object
     */
    onBeforeRebindPOTable: function (oEvent) {
      var oBindingParams = oEvent.getParameter("bindingParams");
      var oBindingParameters = oBindingParams.parameters;

      oBindingParameters.inlinecount = "allpages";

      if(!oBindingParameters.expand) {
        oBindingParameters.expand = "supplierLocationType,receivingLocationType";
      } else{
        oBindingParameters.expand += ",supplierLocationType,receivingLocationType";
      }

      this._changeFilterBeforeRequest(oBindingParams);
      this.addDefaultSorters(oBindingParams.sorter, ["purchaseOrderNo"]);
    },

    /**
     * PO Item SmartTable lifecycle 'beforeRebind' hook event.
     * Add parameters before request.
     * @param {sap.ui.base.Event} oEvent event object
     */
    onBeforeRebindPOItemsTable: function (oEvent) {
      var oBindingParams = oEvent.getParameter("bindingParams");
      var oBindingParameters = oBindingParams.parameters;

      oBindingParameters.inlinecount = "allpages";

      if(!oBindingParameters.expand) {
        oBindingParameters.expand = "supplierLocationType,receivingLocationType";
      } else{
        oBindingParameters.expand += ",supplierLocationType,receivingLocationType";
      }

      this.addDefaultSorters(oBindingParams.sorter, ["purchaseOrderNo", "itemNo"]);
    },

    /**
     * Route to the PO Item details page
     * @param {*} oEvent press event
     */
    onPurchaseOrderItemPressed: function (oEvent) {
      var oContext = oEvent.getSource().getBindingContext();
      var sId = oContext.getProperty("id");

      this.getRouter().navTo("PurchaseOrderItemDetails", {
        id: sId,
      });
    },

    /**
     * Route to the PO details page
     * @param {*} oEvent press event
     */
    onPurchaseOrderPressed: function (oEvent) {
      var oContext = oEvent.getSource().getBindingContext();
      var sId = oContext.getProperty("id");

      this.getRouter().navTo("PurchaseOrderDetails", {
        id: sId,
      });
    },

    /**
     * Update the filters text when filters are changed
     */
    onAssignedFiltersChanged: function () {
      var sStatusText = this.byId("statusText");
      var sText = this.byId("smartFilterBar").retrieveFiltersWithValuesAsText();
      sStatusText.setText(sText);
    },

    // ========================================================================
    // Variant Management
    // ========================================================================

    onAfterVariantLoad: function () {
      var oModel = this.getModel(this.routeName);
      var oSmartFilterBar = this.byId("smartFilterBar");
      var oData = oSmartFilterBar.getFilterData();
      oModel.setProperty("/customFilters", oData._CUSTOM);
    },

    onBeforeVariantSave: function (oEvent) {
      if (oEvent.getParameter("context") === Constants.DEFAULT_VARIANT_FILTER) {
        this.updateCustomFilter();
      }
    },

    onBeforeVariantFetch: function () {
      this.updateCustomFilter();
    },

    updateCustomFilter: function () {
      var oModel = this.getModel(this.routeName);
      var oCustomFilters = oModel.getProperty("/customFilters");
      var oSmartFilterBar = this.byId("smartFilterBar");
      oSmartFilterBar.setFilterData({
        _CUSTOM: oCustomFilters,
      });
    },
  });
});
