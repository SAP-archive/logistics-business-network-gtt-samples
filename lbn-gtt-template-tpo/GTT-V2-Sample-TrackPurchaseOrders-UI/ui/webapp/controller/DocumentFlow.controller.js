sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "sap/ui/core/theming/Parameters",
  "sap/suite/ui/commons/networkgraph/Status",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
  "com/sap/gtt/app/sample/pof/util/AsyncUtils",
  "sap/suite/ui/commons/networkgraph/layout/SwimLaneChainLayout",
], function (BaseController, JSONModel, Parameters, NetworkGraphStatus, Constants, ServiceUtils, RestClient, AsyncUtils, SwimLaneChainLayout) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.DocumentFlow", {

    /**
     * Initialize graph view model.
     */
    initModel: function () {
      this._oGraphModel = new JSONModel();
      this._oGraphModel.setSizeLimit(10000);
      this.setModel(this._oGraphModel, "graph");
    },

    /**
     * Initialize graph control and add custom statuses.
     */
    initControls: function () {
      var oNetworkGraph = this.byId("networkGraph");
      oNetworkGraph.setLayoutAlgorithm(new SwimLaneChainLayout());

      this.addNetworkGraphStatuses();
    },

    /**
     * Refresh document flow data.
     */
    refresh: function () {
      this.clearDocumentFlow();

      var oDocumentView = this.getView();
      oDocumentView.setBusy(true);

      var oDocumentFlowRequest = this.getDocumentFlowRequest();
      AsyncUtils.finally(oDocumentFlowRequest, function () {
        oDocumentView.setBusy(false);
        this._preselectFocusNode();
      }.bind(this));
    },

    /**
     * Clear graph model.
     */
    clearDocumentFlow: function () {
      this._oGraphModel.setProperty("/", {});
    },

    /**
     * Return document flow request.
     * This method is used by document flow that is in PO detail and PO Item detail.
     * The rest api and parameters are depended on semantic object.
     * @returns {Promise} document flow promise
     */
    getDocumentFlowRequest: function () {
      var oRestService = ServiceUtils.getDataSource("restService"),
        oBindingContext = this.getView().getBindingContext(),
        sUrl, oParameters = {};

      if (this.isPOFocusGroup()) {
        sUrl = ServiceUtils.getUrl(oRestService.uri + "/documentFlow");
        oParameters.purchaseOrderId = oBindingContext.getProperty("id");
      } else {
        sUrl = ServiceUtils.getUrl(oRestService.uri + "/documentItemFlow");
        oParameters.purchaseOrderItemId = oBindingContext.getProperty("id");
        oParameters.purchaseOrderId = oBindingContext.getProperty("purchaseOrder/id");
      }
      var oDocumentFlowRequest = RestClient.get(sUrl, {
        params: oParameters,
      });

      oDocumentFlowRequest
        .then(function (oData) {
          this._oGraphModel.setProperty("/", oData);
        }.bind(this), function (oError) {
          this.handleServerError(oError);
        }.bind(this));

      return oDocumentFlowRequest;
    },

    /**
     * Navigate to detail page.
     *  - in PO detail page - to PO Item detail.
     *  - in PO Item detail page - to PO detail.
     * @param {sap.ui.base.Event} oEvent event object
     */
    navToDetailPage: function (oEvent) {
      var oBindingCtx = oEvent.getSource().getBindingContext("graph"),
        sDetailRouteName,
        oRouterConfig = {
          id: oBindingCtx.getProperty("id"),
        };

      switch (oBindingCtx.getObject("group")) {
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER:
          sDetailRouteName = Constants.ROUTES_NAME.PURCHASE_ORDER;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM:
          sDetailRouteName = Constants.ROUTES_NAME.PURCHASE_ORDER_ITEM;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.INBOUND_DELIVERY_ITEM:
          sDetailRouteName = Constants.ROUTES_NAME.INBOUND_DELIVERY_ITEM;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.SHIPMENT:
          sDetailRouteName = "";
          break;
      }

      // Uncomment once navigation to TS is available
      // if(!sDetailRouteName) {
      //   this.navigateToShipmentApp(oRouterConfig.id);
      // } else {
      this.getRouter().navTo(sDetailRouteName, oRouterConfig);
      // }
    },

    /**
     * Cross-app navigation to the external Shipment application.
     * @param {string} sShipmentId shipment id
     */
    navigateToShipmentApp: function (sShipmentId) {
      var oCrossAppNav = sap.ushell.Container.getService("CrossApplicationNavigation");
      var oNavObj = {
        target: {
          semanticObject: "Shipment",
          action: "track",
        },
      };
      oCrossAppNav.isNavigationSupported([oNavObj])
        .done(function (aResults) {
          if (aResults[0].supported) {
            var sExternalHref = oCrossAppNav.hrefForExternal(oNavObj);
            var sPath = "&/Shipment(guid'" + sShipmentId + "')";
            var sUrl = window.location.href.split("#")[0] + sExternalHref + sPath;

            sap.m.URLHelper.redirect(sUrl, true);
          } else {
            this.displayErrorMessageBox(false, true, this.getText("crossNavigationNotSupportedMsg"));
          }
        }.bind(this));
    },

    /**
     * Add custom statuses for the graph.
     */
    addNetworkGraphStatuses: function () {
      var oNetworkGraph = this.byId("networkGraph");
      var aStatusConfigList = [{
        key: "ValueStatusError",
        parameter: "sapUiErrorColor",
      }, {
        key: "ValueStatusWarning",
        parameter: "sapUiWarningColor",
      }, {
        key: "ValueStatusSuccess",
        parameter: "sapUiSuccessColor",
      }];

      aStatusConfigList.forEach(function (oStatusConf) {
        var oStatus = new NetworkGraphStatus({
          key: oStatusConf.key,
          contentColor: Parameters.get(oStatusConf.parameter),
        });
        oNetworkGraph.addStatus(oStatus);
      });
    },

    /**
     * Check if the semantic object is PO.
     * @returns {boolean} true | false
     */
    isPOFocusGroup: function () {
      return this.getViewModel().getProperty(Constants.FOCUS_GROUP_PATH) === Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER;
    },

    /**
     * Set the focus group number.
     * @param {int} iFocusGroupNum node group number
     */
    setFocusGroup: function (iFocusGroupNum) {
      this.getViewModel().setProperty(Constants.FOCUS_GROUP_PATH, iFocusGroupNum);
    },

    /**
     * Return appropriate icon based on the group number.
     * @param {int} iGroup node group number
     * @returns {string} icon src
     */
    getGraphGroupIcon: function (iGroup) {
      switch (iGroup) {
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER:
          return "sap-icon://documents";
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM:
          return "sap-icon://document-text";
        case Constants.DOCUMENT_FLOW_GROUP.INBOUND_DELIVERY_ITEM:
          return "sap-icon://sap-box";
        case Constants.DOCUMENT_FLOW_GROUP.INBOUND_DELIVERY:
          return "sap-icon://shipping-status";
        case Constants.DOCUMENT_FLOW_GROUP.SHIPMENT:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-container-loading";
        case Constants.DOCUMENT_FLOW_GROUP.RESOURCE:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-container";
        default:
          return "";
      }
    },

    /**
     * Return appropriate label for the attribute
     * @param {object} oAttribute attribute object
     * @returns {string} attribule label
     */
    getGraphAttributeLabel: function (oAttribute) {
      var sEntitySet;

      switch (oAttribute.group) {
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER:
          sEntitySet = Constants.ENTITY_TYPES.PURCHASE_ORDER;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM:
          sEntitySet = Constants.ENTITY_TYPES.PURCHASE_ORDER_ITEM;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.INBOUND_DELIVERY_ITEM:
          sEntitySet = Constants.ENTITY_TYPES.INBOUND_DELIVERY_ITEM;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.INBOUND_DELIVERY:
          sEntitySet = Constants.ENTITY_TYPES.INBOUND_DELIVERY;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.SHIPMENT:
          sEntitySet = Constants.ENTITY_TYPES.SHIPMENT;
          break;
        case Constants.DOCUMENT_FLOW_GROUP.RESOURCE:
          sEntitySet = Constants.ENTITY_TYPES.RESOURCE;
          break;
        default:
          sEntitySet = "";
          break;
      }
      return this.getPropertyLabelText(oAttribute.propertyName, sEntitySet);
    },

    getAttributeValue: function (oAttribute) {
      if (oAttribute.propertyName === Constants.PLANNED_DELIVERY_DATE_PROP) {
        var oDateConfigurationSettings = {
          date    : new Date(+oAttribute.value),
          pattern : "MMM dd, yyyy",
        };
        return this.formatter.formatDateRange(oDateConfigurationSettings);
      } else if (oAttribute.propertyName === Constants.PROCESS_STATUS_CODE_PROP) {
        return this.formatter.getCodeListDescriptionFromI18n.call(this, oAttribute.value, "CO_ProcessStatus_" + oAttribute.value + "_NAME");
      } else if (oAttribute.propertyName === Constants.EXECUTION_STATUS_CODE_PROP) {
        return this.formatter.getCodeListDescriptionFromI18n.call(this, oAttribute.value, "CO_ExecutionStatus_" + oAttribute.value + "_NAME");
      } else {
        return this.formatter.formatAmountUnitField(oAttribute.value, oAttribute.uom);
      }
    },

    /**
     * Format the state of action button of node.
     * Return true - if it should be available.
     * @param {object} oNode node
     * @return {boolean} true | false
     */
    formatActionBtnState: function (oNode) {
      if(!oNode) {
        return false;
      }
      var sTrackingIdType = oNode.trackingIdType,
        sGroup = oNode.group,
        bIfNodeHasLink = sTrackingIdType === Constants.TRACKING_ID_TYPE.INBOUND_DELIVERY
          || sTrackingIdType === Constants.TRACKING_ID_TYPE.INBOUND_RESOURCE
          || sTrackingIdType === Constants.TRACKING_ID_TYPE.INBOUND_SHIPMENT;
      return this.getViewModel().getProperty(Constants.FOCUS_GROUP_PATH) !== +sGroup && !bIfNodeHasLink;
    },

    // ======================================================================
    // Private methods.
    // ======================================================================
    _preselectFocusNode: function () {
      var oNetworkGraph = this.byId("networkGraph"),
        sKey = this.isPOFocusGroup() ? Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER : Constants.DOCUMENT_FLOW_GROUP.PURCHASE_ORDER_ITEM;
      oNetworkGraph.getNodeByKey(sKey).setSelected(true);
    },

    // ======================================================================
    // Events
    // ======================================================================

  });
}
);
