sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "sap/ui/model/Filter",
  "sap/ui/model/FilterOperator",
], function (BaseController, JSONModel, Constants, Filter, FilterOperator) {
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
      var oListModel = new JSONModel({
        customFilters: {
          "inboundDeliveryItems.inboundDelivery.inboundDeliveryNo":{
            inputValue: "",
          },
          "inboundDeliveryItems.inboundDelivery.shipmentTPs.shipment.shipmentNo":{
            inputValue: "",
          },
        }});
      this.setModel(oListModel, this.routeName);
    },

    initViewModel: function () {
      BaseController.prototype.initViewModel.call(this);

      this.getViewModel().setData({
        selectedTabFitlerKey: "purchaseOrderTab",
        purchaseOrderAmount: "0",
        purchaseOrderItemsAmount: "0"});
    },

    initControls: function () {
      var oCompletionRate = this.byId("orderCompletionRate");
      var oItemCompletionRate = this.byId("itemCompletionRate");
      this.initCompletionRate([oCompletionRate, oItemCompletionRate], Constants.TWO_DECIMALS_AFTER_DOT);
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
     * Add custom filters to PO.
     * @param {Object[]} aBindingFilters smart table binding filters
     * @returns {Object[]} return the changed filters array
     */
    _addCustomFilters: function (aBindingFilters) {
      var oCustomFilters = this.getModel(this.routeName).getProperty("/customFilters");
      var aCustomFilters = [],
        oResultFilter;
      for (var key in oCustomFilters) {
        if (oCustomFilters[key] && oCustomFilters[key].filter) {
          aCustomFilters.push(oCustomFilters[key].filter);
        }
      }
      if (aCustomFilters.length) {
        if(!aBindingFilters.length) {
          oResultFilter = new Filter(aCustomFilters, true);
        } else {
          aBindingFilters.push(new Filter(aCustomFilters, true));
          oResultFilter = new Filter(aBindingFilters, true);
        }
      }
      return oResultFilter;
    },

    /**
     * Change the filter path if items should be filtered by child nodes.
     * @param {object} oBindingParams binding parameters
     */
    _changeFilterBeforeRequest: function (oBindingParams) {
      var oFiltersParam = this._addCustomFilters(oBindingParams.filters);
      if(!oFiltersParam) {
        return;
      }
      this._setFilterPath(oFiltersParam);
      // update binding filters
      oBindingParams.filters = oFiltersParam;
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
        if(oFilter.sPath === Constants.PO_ITEM_DELIVERY_NO) {
          oFilter.sPath = Constants.PO_DELIVERY_NO;
        }
        if(oFilter.sPath === Constants.PO_ITEM_SHIPMENT_NO) {
          oFilter.sPath = Constants.PO_SHIPMENT_NO;
        }
      }
    },

    // ======================================================================
    // Events
    // ======================================================================

    /**
     * Event when change custom filter input, set Filter to model
     * @param {object} oEvent event object
     */
    onItemChanged: function (oEvent) {
      var oSource = oEvent.getSource();
      var sFilterKey = oSource.data("key");
      var sNewValue = oEvent.getParameter("newValue").trim();
      var oFilter = null;

      if (sNewValue) {
        oFilter = new Filter({
          path: sFilterKey.replace(/\./g, "/"),
          operator: FilterOperator.Contains,
          value1: sNewValue,
        });
      }

      var oModel = this.getModel(this.routeName);
      if (oFilter) {
        oModel.setProperty(Constants.CUSTOM_FILTERTS_PATH + "/" + sFilterKey + "/filter", oFilter);
      } else {
        oModel.setProperty(Constants.CUSTOM_FILTERTS_PATH + "/" + sFilterKey + "/filter", null);
      }

      var oSmartFilterBar = this.byId("smartFilterBar");
      oSmartFilterBar.fireFilterChange(oEvent);
    },

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

      // add default sorting
      this.addDefaultSorters(oBindingParams.sorter, ["purchaseOrderNo", "itemNo"]);

      // update binding filter
      var oFiltersParam = this._getResultPOIFilters(oBindingParams.filters);
      if(!oFiltersParam) {
        return;
      }
      oBindingParams.filters = oFiltersParam;
    },

    /**
     * Add custom filters to PO Items.
     * @param {Object[]} aBindingFilters binding filters
     * @returns {sap.ui.model.Filter} oResultFilter result filter for the PO Item binding
     */
    _getResultPOIFilters: function (aBindingFilters) {
      var oCustomFilters = this.getModel(this.routeName).getProperty("/customFilters");
      var aCustomFilters = [],
        oResultFilter;
      for (var key in oCustomFilters) {
        if (oCustomFilters[key] && oCustomFilters[key].filter) {
          if(oCustomFilters[key].filter.sPath === Constants.PO_DELIVERY_NO) {
            oCustomFilters[key].filter.sPath = Constants.PO_ITEM_DELIVERY_NO;
          }
          if(oCustomFilters[key].filter.sPath === Constants.PO_SHIPMENT_NO) {
            oCustomFilters[key].filter.sPath = Constants.PO_ITEM_SHIPMENT_NO;
          }
          aCustomFilters.push(oCustomFilters[key].filter);
        }
      }
      if (aCustomFilters.length) {
        if(!aBindingFilters.length) {
          oResultFilter = new Filter(aCustomFilters, true);
        } else {
          aBindingFilters.push(new Filter(aCustomFilters, true));
          oResultFilter = new Filter(aBindingFilters, true);
        }
      }

      return oResultFilter;
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
      oModel.setProperty(Constants.CUSTOM_FILTERTS_PATH, oData._CUSTOM);

      oSmartFilterBar.search();
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
      var oCustomFilters = oModel.getProperty(Constants.CUSTOM_FILTERTS_PATH);
      var oSmartFilterBar = this.byId("smartFilterBar");
      oSmartFilterBar.setFilterData({
        _CUSTOM: oCustomFilters,
      });
    },
  });
});
