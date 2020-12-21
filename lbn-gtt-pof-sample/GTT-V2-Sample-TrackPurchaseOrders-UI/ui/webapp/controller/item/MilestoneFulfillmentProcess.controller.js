sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "sap/suite/ui/commons/ProcessFlowZoomLevel",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
  "com/sap/gtt/app/sample/pof/util/AsyncUtils",
], function (BaseController, JSONModel, ProcessFlowZoomLevel, Constants, ServiceUtils, RestClient, AsyncUtils) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.item.MilestoneFulfillmentProcess", {

    initModel: function () {
      this._oMilestoneProcessModel = new JSONModel();
      this._oMilestoneProcessModel.setSizeLimit(100);
      this.setModel(this._oMilestoneProcessModel, "milestoneProcess");
    },

    initControls: function () {
      this.byId("processflow").setZoomLevel(ProcessFlowZoomLevel.One);
    },

    refresh: function () {
      this._clearMilestoneProcessModel();
      this.byId("processflow").updateModel();
      this.getView().setBusy(true);

      AsyncUtils.finally(
        this.getMilestoneProcessRequest(),
        function () {
          this.getView().setBusy(false);
        }.bind(this));
    },

    _clearMilestoneProcessModel: function () {
      this._oMilestoneProcessModel.setProperty("/", {});
    },

    getMilestoneProcessRequest: function () {
      var oRestService = ServiceUtils.getDataSource("restService"),
        oBindingContext = this.getView().getBindingContext(),
        sFulfillmentProcessUrl = ServiceUtils.getUrl(oRestService.uri.concat("/fulfillmentProcessFlow"));

      var oMilestoneProcessRequest = RestClient.get(sFulfillmentProcessUrl, {
        params: {
          purchaseOrderItemId: oBindingContext.getProperty("id"),
        },
      });

      oMilestoneProcessRequest
        .then(
          function (oData) {
            this._oMilestoneProcessModel.setProperty("/", oData);
          }.bind(this),
          function (oError) {
            this.handleServerError(oError);
          }.bind(this));

      return oMilestoneProcessRequest;
    },

    getProcessLaneIcon: function (oLane) {
      switch (oLane.name) {
        case Constants.MILESTONE_PROCESS_NAME.CREATED:
          return "sap-icon://create";
        case Constants.MILESTONE_PROCESS_NAME.DELIVERY_CREATED:
          return "sap-icon://create-form";
        case Constants.MILESTONE_PROCESS_NAME.CONFIRMED:
          return "sap-icon://approvals";
        case Constants.MILESTONE_PROCESS_NAME.RECEIPT:
        case Constants.MILESTONE_PROCESS_NAME.DELIVERY_COMPLETE:
          return "sap-icon://complete";
        case Constants.MILESTONE_PROCESS_NAME.DELIVERY_RECEIPT:
          return "sap-icon://receipt";
        case Constants.MILESTONE_PROCESS_NAME.DELETED:
          return "sap-icon://delete";
        default:
          return "";
      }
    },

    getProcessLaneState: function (oLane) {
      if (oLane.name === Constants.MILESTONE_PROCESS_NAME.CREATED) {
        return [{
          state: "Positive",
          value: oLane.total,
        }];
      } else if (oLane.name === Constants.MILESTONE_PROCESS_NAME.DELETED) {
        return [{
          state: "Negative",
          value: oLane.total,
        }];
      } else {
        return [{
          state: "Positive",
          value: oLane.count,
        }, {
          state: "Neutral",
          value: oLane.total - oLane.count,
        }];
      }
    },

    getMilestoneProcessLabel: function (oLane) {
      return this.getText(oLane.name, [oLane.count, oLane.total]);
    },
  });
});
