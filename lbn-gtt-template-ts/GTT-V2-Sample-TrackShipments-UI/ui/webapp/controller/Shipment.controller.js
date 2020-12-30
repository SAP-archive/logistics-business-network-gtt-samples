sap.ui.define(
  [
    "./BaseDetailController",
    "sap/ui/core/Fragment",
    "../util/ServiceUtils",
    "../util/RestClient",
  ],
  function (
    BaseDetailController,
    Fragment,
    ServiceUtils,
    RestClient
  ) {
    "use strict";

    return BaseDetailController.extend("com.sap.gtt.app.sample.sst.controller.Shipment", {
      routeName: "shipment",

      initControls: function () {
        this.initGenericTags(["executionStatusGenericTag", "processStatusGenericTag"]);
        this.registerEvents(this.routeName);
      },

      routePatternMatched: function (oEvent) {
        var model = this.getModel(this.routeName);
        model.setProperty("/shipmentNo", null);

        var args = oEvent.getParameter("arguments");
        var id = args.id;
        model.setProperty("/shipmentId", id);

        // Bind the view with an entry
        var odataModel = this.getModel();
        odataModel.metadataLoaded().then(function () {
          var entitySetKey = odataModel.createKey("Shipment", {
            id: id,
          });
          this.bindView(entitySetKey);
        }.bind(this));
      },

      getExpandList: function () {
        return [
          "departureLocation",
          "arrivalLocation",
          "shippingType/localized",
          "trafficDirection/localized",
          "incoterms/localized",
          "transportMeans/localized",
          "processStatus/localized",
          "executionStatus/localized",
        ];
      },

      /**
       * Update events by status
       */
      updateView: function () {
        var model = this.getModel(this.routeName);

        var isCompleted = this.isShipmentCompleted();
        model.setProperty("/isCompleted", isCompleted);

        if (!isCompleted) {
          this.updateNextStop();
        }
        this.updateShipmentEventsByStatus();

        // refresh subsections
        this.refreshSubSection("trackingTimelineView");

        var viewModel = this.getModel("view");
        viewModel.setProperty(
          "/lastUpdatedAtTime",
          viewModel.getProperty("/lastUpdatedOn" + this.getView().getElementBinding().getPath())
        );
      },

      /**
       * Trigged by pressing the refresh button in order to refresh the view
       */
      onRefreshPressed: function () {
        var view = this.getView();
        view.getElementBinding().refresh(true);
      },

      updateNextStop: function () {
        var model = this.getModel(this.routeName);

        var request = this.createGetRequestWithShipmentId("nextStop");
        request.then(function (data) {
          model.setProperty("/nextStop", data);
        });
      },

      updateShipmentEventsByStatus: function () {
        var model = this.getModel(this.routeName);

        var request = this.createGetRequestWithShipmentId("eventsByStatus");
        request.then(function (data) {
          model.setProperty("/eventStatusTypeCount", data.length);
          model.setProperty("/eventsCount", data.reduce(
            function (total, current) {
              return total + current.count;
            }, 0
          ));
          model.setProperty("/eventsByStatusSet", this.groupEventsByStatus(data));
        }.bind(this));
      },

      /**
       * Group the events for chart on the page header
       *
       * @param {object[]} events The restructured events to be grouped
       * @returns {object[]} The grouped events
       */
      groupEventsByStatus: function (events) {
        var groupNo = {
          DELAYED: 0,
          OVERDUE: 1,
          REPORTED: 2,
          PLANNED: 3,
        };
        return events.sort(function (a, b) {
          return groupNo[a.eventStatus_code] - groupNo[b.eventStatus_code];
        });
      },

      /**
       * Add "Visibility Provider" row in the reference documents table
       *
       * @param {sap.ui.base.EventProvider} source Event source
       * @param {string} trackId trackId
       */
      addVpRowInRefDocumentsTable: function (source, trackId) {
        if (!trackId) {
          // Remove visibility provider row
          this.byId("vpRow").destroy();
          source.data("isVpRowAdded", false);
          return;
        } else if (source.data("isVpRowAdded")) {
          // Update trackId
          this.byId("trackIdText").setText(trackId);
          return;
        }

        // Create & Add new row in current table
        Fragment.byId(
          this.createId("referenceDocumentsFragment"),
          "referenceDocumentsTable"
        ).addItem(
          new sap.m.ColumnListItem(this.createId("vpRow"), {
            cells: [
              new sap.m.Text({
                text: this.getText("visibilityProvider"),
              }),
              new sap.m.Text({
                id: this.createId("trackIdText"),
                text: trackId,
              }),
            ],
          })
        );
        source.data("isVpRowAdded", true);
      },

      isShipmentCompleted: function () {
        var bindingContext = this.getView().getBindingContext();
        return bindingContext.getProperty("executionStatus_code") === "COMPLETED";
      },
    });
  }
);
