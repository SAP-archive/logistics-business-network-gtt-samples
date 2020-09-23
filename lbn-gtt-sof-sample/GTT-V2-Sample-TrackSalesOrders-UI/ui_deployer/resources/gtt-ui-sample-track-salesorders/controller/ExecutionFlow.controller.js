sap.ui.define(
  [
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/suite/ui/commons/library",
    "sap/ui/core/Fragment",
    "sap/ui/Device",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ],
  function (
    BaseController,
    JSONModel,
    CommonLibrary,
    Fragment,
    Device,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    return BaseController.extend("com.sap.gtt.app.sample.sof.controller.ExecutionFlow", {

      initModel: function () {
        var model = new JSONModel();
        this.setModel(model, "executionFlow");
      },

      initControls: function () {
        this.initProcessFlowNode();
      },

      initProcessFlowNode: function () {
        var node = this.byId("processFlowNode");
        node.addEventDelegate({
          onAfterRendering: function (oEvent) {
            var source = oEvent.srcControl;
            if (this.formatter.eventStatus.hasNoHistoricalReporting(source.data("eventStatus"))
              && !source.hasStyleClass("sofProcessFlowNodePlannedEvent")) {
              source.addStyleClass("sofProcessFlowNodePlannedEvent");
            }
          }.bind(this),
        });
      },

      refresh: function () {
        var model = this.getModel("executionFlow");
        model.setProperty("/", {});

        var view = this.getView();
        view.setBusy(true);

        // set zoom level
        var processFlow = this.byId("processflow");
        if (Device.system.phone) {
          processFlow.setZoomLevel(CommonLibrary.ProcessFlowZoomLevel.Four);
        } else {
          processFlow.setZoomLevel(CommonLibrary.ProcessFlowZoomLevel.One);
        }

        var request = this.getExecutionFlowRequest();
        AsyncUtils.finally(request, function () {
          view.setBusy(false);
        });
      },

      getExecutionFlowRequest: function () {
        var model = this.getModel("executionFlow");
        var jsonService = ServiceUtils.getDataSource("restService");
        var bindingContext = this.getView().getBindingContext();
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/executionFlow"));
        var request = RestClient.get(url, {
          params: {
            deliveryItemId: bindingContext.getProperty("id"),
          },
        });

        request.then(function (data) {
          model.setProperty("/", data);
          this.updateProcessFlowHeaderStates();
        }.bind(this), function (error) {
          this.handleServerError(error);
        }.bind(this));

        return request;
      },

      updateProcessFlowHeaderStates: function () {
        var processflow = this.byId("processflow");
        processflow.updateModel();
        var nodes = this.getModel("executionFlow").getProperty("/nodes") || [];
        var lanes = processflow.getLanes();
        nodes.forEach(function (node, index) {
          lanes[index].setState([{
            state: this.formatter.eventStatus.getProcessFlowHeaderState(node.eventStatus),
            value: 1,
          }]);
        }.bind(this));
      },

      onZoomIn: function () {
        var processFlow = this.byId("processflow");
        processFlow.zoomIn();
        this.updateProcessFlowHeaderStates();
      },

      onZoomOut: function () {
        var processFlow = this.byId("processflow");
        processFlow.zoomOut();
        this.updateProcessFlowHeaderStates();
      },

      onNodePressed: function (oEvent) {
        var source = oEvent.getParameters();
        this.selectedNode = source;
        if (!this.eventsPopover) {
          Fragment.load({
            id: "eventsPopover",
            name: "com.sap.gtt.app.sample.sof.view.fragments.EventsPopover",
            controller: this,
          }).then(function (popover) {
            this.eventsPopover = popover;
            this.getView().addDependent(this.eventsPopover);
            this.eventsPopover.openBy(source);
          }.bind(this));
        } else {
          this.eventsPopover.openBy(source);
        }
      },

      historicalEventsPopoverAfterOpen: function () {
        var context = this.selectedNode.getBindingContext("executionFlow");
        this.updateHistoricalEvents(context.getModel(), {
          eventId: context.getProperty("eventId"),
        });
      },

      getETAText: function (datetime) {
        return this.getText("estimatedArrivalTime").concat(":\n").concat(datetime);
      },
    });
  }
);
