sap.ui.define(
  [
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/suite/ui/commons/library",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ],
  function (
    BaseController,
    JSONModel,
    CommonLibrary,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    return BaseController.extend("com.sap.gtt.app.sample.sof.controller.Milestone", {

      initModel: function () {
        var model = new JSONModel();
        this.setModel(model, "milestone");
      },

      initControls: function () {
        this.initProcessFlow();
      },

      initProcessFlow: function () {
        this.byId("processflow").setZoomLevel(CommonLibrary.ProcessFlowZoomLevel.One);
      },

      refresh: function () {
        var view = this.getView();
        view.setBusy(true);

        var model = this.getModel("milestone");
        var jsonService = ServiceUtils.getDataSource("restService");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/fulfillmentProcessFlow"));
        var bindingContext = this.getView().getBindingContext();
        var request = RestClient.get(url, {
          params: {
            salesOrderItemId: bindingContext.getProperty("id"),
          },
        });
        request.then(function (data) {
          model.setProperty("/", data);
          this.byId("processflow").updateModel();
        }.bind(this), function (error) {
          this.handleServerError(error);
        }.bind(this));

        AsyncUtils.finally(request, function () {
          view.setBusy(false);
        });
      },
    });
  }
);
