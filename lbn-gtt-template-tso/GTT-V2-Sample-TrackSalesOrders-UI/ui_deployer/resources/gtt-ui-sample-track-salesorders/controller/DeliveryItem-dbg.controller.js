sap.ui.define(
  [
    "./BaseDetailController",
    "sap/base/Log",
    "../util/ServiceUtils",
    "../util/RestClient",
  ],
  function (
    BaseDetailController,
    Log,
    ServiceUtils,
    RestClient
  ) {
    "use strict";

    return BaseDetailController.extend("com.sap.gtt.app.sample.sof.controller.DeliveryItem", {
      routeName: "deliveryItem",

      initControls: function () {
        this.initGenericTags(["lastActivityGenericTag", "processStatusGenericTag"]);
        this.registerEvents(this.routeName);
      },

      initModel: function () {
        BaseDetailController.prototype.initModel.call(this, arguments);

        var model = this.getModel(this.routeName);
        model.setProperty("/businessReferenceNo", []);
      },

      subscribeEvents: function () {
        this.getEventBus().subscribe("deliveryItem", "updatePlannedArrivalETA", this.updatePlannedArrivalETA, this);
      },

      routePatternMatched: function (oEvent) {
        var model = this.getModel(this.routeName);
        var args = oEvent.getParameter("arguments");
        var urlParams = args["?params"];
        model.setProperty("/urlParams", urlParams);
        model.setProperty("/referenceDocuments", []);

        // Bind the view with an entry
        var odataModel = this.getModel();
        odataModel.metadataLoaded().then(function () {
          var entitySetKey = odataModel.createKey("DeliveryItem", {
            id: args.id,
          });
          this.bindView(entitySetKey);
        }.bind(this));
      },

      getExpandList: function () {
        return [
          "destinationLocation",
          "salesOrderItem",
          "processStatus/localized",
          "executionStatus/localized",
          "lastVPLocationType/localized",
          "incoterms/localized",
        ];
      },

      /**
       * update fulfillmentstatus, reference documents, execution flow and map
       */
      updateView: function () {
        this.clearPlannedArrivalETA();
        this.updateDestination();
        this.updateDeliveryItemFulfillmentStatus();
        this.updateReferenceDocuments();

        // refresh subsections
        this.refreshSubSection("executionFlowView");
        this.refreshSubSection("mapView");
      },

      clearPlannedArrivalETA: function () {
        var model = this.getModel(this.routeName);
        model.setProperty("/destination", null);
        model.setProperty("/earliestETA", null);
        model.setProperty("/earliestETATimeZone", "");
        model.setProperty("/plannedArrivalTime", null);
      },

      /**
       * Update planned arrival/ETA for destination
       *
       * @param {string} channelId Channel ID
       * @param {string} eventId Event ID
       * @param {object[]} routes Routes
       */
      updatePlannedArrivalETA: function (channelId, eventId, routes) {
        if (routes.length === 0) {
          return;
        }

        var model = this.getModel(this.routeName);
        var destination = model.getProperty("/destination");
        if (destination === null) {
          return;
        }

        var earliestETASpot = routes.reduce(function (previous, current) {
          return previous.concat(current.plannedSpots);
        }, []).filter(function (plannedSpot) {
          return (
            plannedSpot.eventType === "Arrival" &&
            plannedSpot.locationAltKey === destination.locationAltKey
          );
        }).sort(function (a, b) {
          if (a.estimatedArrival === null) {
            return 1;
          } else if (b.estimatedArrival === null) {
            return -1;
          }
          return new Date(a.estimatedArrival.estimatedArrivalTime) - new Date(b.estimatedArrival.estimatedArrivalTime);
        })[0];

        if (earliestETASpot) {
          var estimatedArrival = earliestETASpot.estimatedArrival;
          if (estimatedArrival !== null) {
            model.setProperty("/earliestETA", new Date(estimatedArrival.estimatedArrivalTime));
            model.setProperty("/earliestETATimeZone", estimatedArrival.estimatedArrivalTimeZone);
          }
          model.setProperty("/plannedArrivalTime", new Date(earliestETASpot.plannedBusinessTimestamp));
        }
      },

      updateLocationQuickViewData: function () {
        var model = this.locationQuickView.getModel();
        var destination = this.getModel(this.routeName).getProperty("/destination");
        model.setProperty("/", destination);
      },

      updateDestination: function () {
        var context = this.getView().getBindingContext();
        var destinationLocation = context.getProperty("destinationLocation");
        this.getModel(this.routeName).setProperty("/destination", destinationLocation);
      },

      updateDeliveryItemFulfillmentStatus: function () {
        var model = this.getModel(this.routeName);

        var jsonService = ServiceUtils.getDataSource("restService");
        var bindingContext = this.getView().getBindingContext();
        var deliveryItemId = bindingContext.getProperty("id");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/fulfillmentStatus"));
        var request = RestClient.get(url, {
          params: {
            deliveryItemId: deliveryItemId,
          },
        });

        request.then(function (data) {
          model.setProperty("/actualEventsCount", data.length);
          model.setProperty("/fulfillmentStatusSet", this.groupEventsByStatus(data));
        }.bind(this), function (error) {
          this.handleServerError(error);
          Log.error(error.data);
        });
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

      updateReferenceDocuments: function () {
        var model = this.getModel(this.routeName);

        var jsonService = ServiceUtils.getDataSource("restService");
        var bindingContext = this.getView().getBindingContext();
        var deliveryItemId = bindingContext.getProperty("id");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/carrierRefDocuments"));
        var request = RestClient.get(url, {
          params: {
            deliveryItemId: deliveryItemId,
          },
        });

        request.then(function (data) {
          model.setProperty("/referenceDocuments", data.filter(
            function (referenceDocument) {
              return referenceDocument.docType_code !== "VP";
            }
          ));
        }, function (error) {
          this.handleServerError(error);
          Log.error(error.data);
        }.bind(this));
      },
    });
  }
);
