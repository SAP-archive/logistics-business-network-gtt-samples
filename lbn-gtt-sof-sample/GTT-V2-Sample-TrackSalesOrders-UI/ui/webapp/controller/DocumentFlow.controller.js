sap.ui.define(
  [
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/suite/ui/commons/networkgraph/layout/SwimLaneChainLayout",
    "sap/ui/core/theming/Parameters",
    "sap/suite/ui/commons/networkgraph/Status",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ],
  function (
    BaseController,
    JSONModel,
    SwimLaneChainLayout,
    Parameters,
    NetworkGraphStatus,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    return BaseController.extend("com.sap.gtt.app.sample.sof.controller.DocumentFlow", {

      initModel: function () {
        var model = new JSONModel();
        model.setSizeLimit(10000);
        this.setModel(model, "graph");
      },

      initControls: function () {
        var networkGraph = this.byId("networkGraph");
        networkGraph.setLayoutAlgorithm(new SwimLaneChainLayout());
        this.addNetworkGraphStatuses();

        this.byId("elementAttribute").addEventDelegate({
          onAfterRendering: function (oEvent) {
            var source = oEvent.srcControl;
            source.$().css("height", "15px");
            var context = source.getBindingContext("graph");
            if (context.getProperty("propertyName") === "netValue") {
              source.$().find(".sapSuiteUiCommonsNetworkGraphDivNodeValues").find(".sapSuiteUiCommonsNetworkGraphDivNodeText").css("font-weight", "bold");
            }
          },
        });
      },

      refresh: function () {
        this.clearDocumentFlow();

        var view = this.getView();
        view.setBusy(true);

        var request = this.getDocumentFlowRequest();
        AsyncUtils.finally(request, function () {
          view.setBusy(false);
        });
      },

      clearDocumentFlow: function () {
        var model = this.getModel("graph");
        model.setProperty("/", {});
      },

      getDocumentFlowRequest: function () {
        var model = this.getModel("graph");
        var jsonService = ServiceUtils.getDataSource("restService");
        var bindingContext = this.getView().getBindingContext();
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/documentFlow"));
        var request = RestClient.get(url, {
          params: {
            salesOrderId: bindingContext.getProperty("id"),
          },
        });
        request.then(function (data) {
          model.setProperty("/", data);
        }, function (error) {
          this.handleServerError(error);
        }.bind(this));

        return request;
      },

      navToDetailPage: function (oEvent) {
        var viewBindingContext = this.getView().getBindingContext();
        var bindingContext = oEvent.getSource().getBindingContext("graph");
        var config = {
          id: bindingContext.getProperty("id"),
          params: {
            salesOrderId: viewBindingContext.getProperty("id"),
          },
        };

        var group = bindingContext.getProperty("group");
        if (group === 2) {
          this.getRouter().navTo("salesOrderItem", config);
        }
        if (group === 3) {
          this.getRouter().navTo("deliveryItem", config);
        }
      },

      addNetworkGraphStatuses: function () {
        var networkGraph = this.byId("networkGraph");
        var valueStatusList = [{
          key: "ValueStatusError",
          parameter: "sapUiErrorColor",
        }];

        valueStatusList.forEach(function (item) {
          var status = new NetworkGraphStatus({
            key: item.key,
            contentColor: Parameters.get(item.parameter),
          });
          networkGraph.addStatus(status);
        });
      },
    });
  }
);
