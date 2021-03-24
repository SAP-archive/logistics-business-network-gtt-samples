sap.ui.define(
  [
    "./BaseDetailController",
    "sap/ui/core/Fragment",
  ],
  function (
    BaseDetailController,
    Fragment
  ) {
    "use strict";

    var CONSTANTS = Object.freeze({
      SLASH: "/",
      IS_FREIGHT_UNIT: "isFreightUnit",
    });

    return BaseDetailController.extend("com.sap.gtt.app.sample.sst.controller.Shipment", {
      routeName: "shipment",

      initControls: function () {
        this.initGenericTags(["executionStatusGenericTag", "processStatusGenericTag"]);
        this.registerEvents();
      },

      onBeforeRendering: function () {
        this.getModel("view").setProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT, false);
      },

      routePatternMatched: function (oEvent) {
        this.getEventBus().publish("tracking-timeline", "clear-map");

        // Bind the view with an entry
        var args = oEvent.getParameter("arguments");
        if (args["?params"]) {
          this.getModel("view").setProperty("/urlParams", args["?params"]);
        }

        var odataModel = this.getModel();
        odataModel.metadataLoaded().then(function () {
          var entitySetKey = this.getEntitySetKey(odataModel, args.id);
          this.bindView(entitySetKey);
        }.bind(this));
      },

      getEntitySetKey: function (odataModel, id) {
        return odataModel.createKey("Shipment", {
          id: id,
        });
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
       * Update events status chart and the tracking view
       */
      updateView: function () {
        var model = this.getModel("view");

        var isCompleted = this.isTrackedProcessCompleted();
        model.setProperty("/isCompleted", isCompleted);

        if (!isCompleted) {
          this.updateNextStop();
        }
        this.updateTrackedProcessEventsByStatus();

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
        var model = this.getModel("view");

        var request = this.createGetRequestWithId("nextStop", model.getProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT));
        request.then(function (data) {
          model.setProperty("/nextStop", data);
        });
      },

      updateTrackedProcessEventsByStatus: function () {
        var model = this.getModel("view");

        var request = this.createGetRequestWithId("eventsByStatus", model.getProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT));
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
       * @param {boolean} isFreightUnit `true` if is on delivery item page
       * @param {sap.ui.base.EventProvider} source Event source
       * @param {string} trackId trackId
       */
      addVpRowInRefDocumentsTable: function (isFreightUnit, source, trackId) {
        if (isFreightUnit) {
          return;
        }

        if (!trackId) {
          // Remove visibility provider row
          this.byId("vpRow").destroy();
          source.data("isVpRowAdded", false);
          return;
        } else if (source.data("isVpRowAdded") && this.byId("vpRow")) {
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

      isTrackedProcessCompleted: function () {
        var bindingContext = this.getView().getBindingContext();
        return bindingContext.getProperty("executionStatus_code") === "COMPLETED";
      },
    });
  }
);
