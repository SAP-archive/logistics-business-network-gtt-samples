sap.ui.define(
  [
    "./BaseController",
    "sap/base/util/deepExtend",
    "sap/ui/model/json/JSONModel",
    "sap/ui/core/Fragment",
    "./MapHelper",
    "../constant/Event",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ], function (
    BaseController,
    deepExtend,
    JSONModel,
    Fragment,
    MapHelper,
    Event,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    var CONSTANTS = Object.freeze({
      SLASH: "/",
      TIMEZONES: "timezones",
      IS_FREIGHT_UNIT: "isFreightUnit",
    });

    return BaseController.extend("com.sap.gtt.app.sample.sst.controller.TrackingTimeline", {

      CHANNEL: "tracking-timeline",

      RouteWidth: {
        DEFAULT: 4,
        BOLD: 8,
      },

      initModel: function () {
        var timelineModel = new JSONModel({
          hasWritePrivilege: false,
          hasPlannedEvents: false,
          hasUnplannedEvents: false,
        });
        var mapModel = new JSONModel();

        timelineModel.setSizeLimit(10000);
        mapModel.setSizeLimit(10000);

        this.setModel(timelineModel, "timeline");
        this.setModel(mapModel, "map");
      },

      onBeforeRendering: function () {
        if (!this.getModel("parentView")) {
          var parentModel = this.getParentModel();
          this.setModel(parentModel, "parentView");
        }
      },

      subscribeEvents: function () {
        this.getEventBus().subscribe(this.CHANNEL, "clear-map", this.clearMap.bind(this));
      },

      unsubscribeEvents: function () {
        this.getEventBus().unsubscribe(this.CHANNEL, "clear-map");
      },

      initControls: function () {
        this.initMap();
      },

      initMap: function () {
        var map = this.getControlByFragment("trackingTimelineMap", "geoMap");
        MapHelper.getHereMapKey().then(function () {
          MapHelper.setMapConfiguration(map, "HERE");
        });
      },

      /**
       * Update tracking timeline events
       */
      refresh: function () {
        this.updateTrackingTimelineEvents();
        this.checkUserWritePrivilege().then(function (hasWritePrivilege) {
          this.getModel("timeline").setProperty("/hasWritePrivilege", hasWritePrivilege);
          this.fetchUnplannedEvents();
          this.fetchTimeZones();
        }.bind(this));

        this.updateMap();
      },

      checkUserWritePrivilege: function () {
        if (this.userWritePrivilegeChecked) {
          return Promise.reject();
        }

        return new Promise(function (resolve, reject) {
          var restUri = ServiceUtils.getDataSource("restService").uri;
          RestClient.post(ServiceUtils.getUrl(restUri.concat("/")), {})
            .then(function () {
              resolve(true);
            }, function (error) {
              if (error.response.status === 403) {
                resolve(false);
              } else {
                resolve(true);
              }
            });
          this.userWritePrivilegeChecked = true;
        }.bind(this));
      },


      // ======================================================================
      // Timeline Events
      // ======================================================================

      updateTrackingTimelineEvents: function () {
        var parentModel = this.getModel("parentView");
        var model = this.getModel("timeline");
        model.setProperty("/isTimelineEventsLoaded", false);

        var request = this.createGetRequestWithId("timelineEvents", parentModel.getProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT));
        request.then(function (data) {
          parentModel.setProperty("/deliveryInformation", data.filter(function (event) {
            return event.eventType === "POD";
          })[0]);
          model.setProperty("/timelineEvents", data.map(function (event) {
            if (event.eventStatusCode !== Event.Status.Type.UNPLANNED) {
              model.setProperty("/hasPlannedEvents", true);
            }

            // Put unreported events at the top of the timeline
            // since Timeline control can only sort/group items by dateTime.
            var plannedDate = new Date(event.plannedBusinessTimestamp);
            plannedDate.setFullYear(plannedDate.getFullYear() + 100);

            event.timestampForSorting = event.actualBusinessTimestamp !== null
              ? event.actualBusinessTimestamp
              : plannedDate.toISOString();
            return event;
          }));
        });
        AsyncUtils.finally(request, function () {
          model.setProperty("/isTimelineEventsLoaded", true);
        });
      },

      fetchUnplannedEvents: function () {
        var model = this.getModel("timeline");
        if (model.getProperty("/hasUnplannedEvents")) {
          return;
        }

        var jsonService = ServiceUtils.getDataSource("restService");
        var path = jsonService.uri.concat("/models/Shipment/unplannedEvents");
        if (this.getModel("parentView").getProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT)) {
          path = jsonService.uri.concat("/models/FreightUnit/unplannedEvents");
        }
        var url = ServiceUtils.getUrl(path);

        RestClient.get(url).then(function (unplannedEvents) {
          if (unplannedEvents && unplannedEvents.length) {
            model.setProperty("/hasUnplannedEvents", true);
            this.unplannedEvents = unplannedEvents;
          }
        }.bind(this), function (error) {
          this.handleServerError(error);
        }.bind(this));
      },

      fetchTimeZones: function () {
        var globalJson = this.getModel("globalJson");
        if (globalJson.getProperty("/timezonesLoaded")) {
          return;
        }

        var jsonService = ServiceUtils.getDataSource("restService");
        var url = ServiceUtils.getUrl(
          jsonService.uri.concat(CONSTANTS.SLASH + CONSTANTS.TIMEZONES)
        );

        RestClient.get(url).then(function (data) {
          globalJson.setProperty(CONSTANTS.SLASH + CONSTANTS.TIMEZONES, data.items);
          globalJson.setProperty("/timezonesLoaded", true);
        }, function (error) {
          this.handleServerError(error);
        }.bind(this));
      },

      openReportEventsActionSheet: function (oEvent) {
        var source = oEvent.getSource();

        if (!this.reportEventsActionSheet) {
          Fragment.load({
            id: this.createId("reportEventsActionSheet"),
            name: "com.sap.gtt.app.sample.sst.view.fragments.ReportEventsActionSheet",
            controller: this,
          }).then(function (actionSheet) {
            this.reportEventsActionSheet = actionSheet;
            source.addDependent(this.reportEventsActionSheet);
            this.reportEventsActionSheet.openBy(source);
          }.bind(this));
        } else {
          this.reportEventsActionSheet.openBy(source);
        }
      },

      /**
       * Open report events dialog
       *
       * @param {"Planned"|"Unplanned"} eventStatus Event with status to report
       * @param {object[]} relatedEvents Related events
       */
      openReportEventsDialog: function (eventStatus, relatedEvents) {
        var plannedEvents = relatedEvents.filter(function (event) {
          return event.eventStatusCode !== Event.Status.Type.UNPLANNED;
        });

        var unplannedEvents = this.unplannedEvents.filter(function (event) {
          return !event.eventType.isFromCoreModel;
        });

        var refPlannedEvents = plannedEvents.filter(function (event) {
          return (
            event.eventStatusCode === Event.Status.Type.OVERDUE ||
            event.eventStatusCode === Event.Status.Type.DELAYED ||
            event.eventStatusCode === Event.Status.Type.PLANNED
          );
        });

        this.getOwnerComponent().reportEventsDialog.open({
          eventStatus: eventStatus,
          altKey: this.getView().getBindingContext().getProperty("altKey"),
          plannedEvents: plannedEvents,
          unplannedEvents: unplannedEvents,
          refPlannedEvents: refPlannedEvents,
          timeZones: this.getModel("globalJson").getProperty(CONSTANTS.SLASH + CONSTANTS.TIMEZONES),
        });
      },

      /**
       * Zoom in the position on map by selected event location
       *
       * @param {sap.suite.ui.commons.TimelineItem} selectedItem Selected timeline event
       * @param {number} [step=2] Zoom-in step
       */
      revealLocationOnMap: function (selectedItem, step) {
        var map = Fragment.byId(this.createId("trackingTimelineMap"), "geoMap");
        var location = selectedItem.getBindingContext("timeline").getObject().location;
        var zoomLevel = map.getZoomlevel();

        if (selectedItem.getId() === map.data("currentId")) {
          zoomLevel += (step ? step : 2);
        }

        map.zoomToGeoPosition(location.longitude, location.latitude, zoomLevel);
        map.data("currentId", selectedItem.getId());
      },

      /**
       * Open a delivery itmes popover
       *
       * @param {sap.ui.base.EventProvider} source Event source
       * @param {object[]} deliveryItems Delivery items
       */
      openDeliveryItemsPopover: function (source, deliveryItems) {
        var props = {
          deliveryItems: deliveryItems,
          count: deliveryItems.length,
        };

        if (!this.deliveryItemsPopover) {
          Fragment.load({
            id: this.createId("deliveryItemsPopover"),
            name: "com.sap.gtt.app.sample.sst.view.fragments.DeliveryItemsPopover",
            controller: this,
          }).then(function (popover) {
            this.deliveryItemsPopover = popover;
            this.getView().addDependent(this.deliveryItemsPopover);
            this.deliveryItemsPopover.setModel(new JSONModel(props), "props");
            this.deliveryItemsPopover.openBy(source);
          }.bind(this));
        } else {
          this.deliveryItemsPopover.close();
          this.deliveryItemsPopover.getModel("props").setData(props);
          this.deliveryItemsPopover.openBy(source);
        }
      },

      /**
       * Open a reporting history popover
       *
       * @param {sap.ui.base.EventProvider} source Event source
       * @param {object[]} historicalEvents Historical events
       */
      openReportingHistoryPopover: function (source, historicalEvents) {
        var props = {
          historicalEvents: historicalEvents.sort(function (a, b) {
            return new Date(b.actualTechnicalTimestamp) - new Date(a.actualTechnicalTimestamp);
          }),
          eventsCount: historicalEvents.length,
        };

        if (!this.reportingHistoryPopover) {
          Fragment.load({
            id: this.createId("reportingHistoryPopover"),
            name: "com.sap.gtt.app.sample.sst.view.fragments.ReportingHistoryPopover",
            controller: this,
          }).then(function (popover) {
            this.reportingHistoryPopover = popover;
            this.getView().addDependent(this.reportingHistoryPopover);
            this.reportingHistoryPopover.setModel(new JSONModel(props), "props");
            this.reportingHistoryPopover.openBy(source);
          }.bind(this));
        } else {
          this.reportingHistoryPopover.close();
          this.reportingHistoryPopover.getModel("props").setData(props);
          this.reportingHistoryPopover.openBy(source);
        }
      },

      /**
       * Navigate to delivery item detail page after clicking the delivery item.
       * If the delivery item is NOT in a freight unit, then navigate to "Track Sales Orders" App.
       *
       * @param {object} deliveryItem Delivery item
       */
      handleDeliveryItemPress: function (deliveryItem) {
        if (deliveryItem.isInFreightUnit) {
          this.getRouter().navTo("freightUnit", {
            id: deliveryItem.id,
            params: {
              itemNo: deliveryItem.itemNo,
            },
          });
        } else {
          this.navToExternalDeliveryItemDetailPage(deliveryItem.id);
        }
      },

      // ======================================================================
      // Map
      // ======================================================================

      updateMap: function () {
        var model = this.getModel("map");
        model.setProperty("/isStopsRoutesLoaded", false);

        var dataName = "routes";
        var request = this.mapRequest = this.createGetRequestWithId(dataName, this.getModel("parentView").getProperty(CONSTANTS.SLASH + CONSTANTS.IS_FREIGHT_UNIT));
        request.then(function (data) {
          this.processMapData(request, data);
        }.bind(this));
        AsyncUtils.finally(request, function () {
          if (!request.isCanceled) {
            model.setProperty("/isStopsRoutesLoaded", true);
          }
        });
      },

      clearMap: function () {
        var model = this.getModel("map");
        model.setProperty("/routes", {
          currentLocations: [],
          stopsForVp: [],
          plannedActualRoutes: [],
          stopsWithETA: [],
        });

        if (this.mapRequest) {
          this.mapRequest.isCanceled = true;
        }
      },

      /**
       * Create the planned route, actual route, current location and stops
       *  @param {Promise} request The map request
       *  @param {object} data The draft data for routes and stops
       */
      processMapData: function (request, data) {
        if (request.isCanceled) {
          return;
        }

        var isCompleted = this.getModel("parentView").getProperty("/isCompleted");
        var positionList = []; // for zooming map
        data.plannedActualRoutes = [];
        data.currentLocations = [];

        if (data.actualRoute.length) {
          this.processActualRouteCurrentLocation(isCompleted, data, positionList);
        }

        if (!isCompleted && data.plannedRoute.length) {
          this.processPlannedRoute(data, positionList);
        }

        this.processStops(isCompleted, data, positionList);

        var model = this.getModel("map");
        model.setProperty("/routes", data);

        this.zoomRouteAndStopsToAreas(positionList);
      },

      /**
       * Create an acutal route and a current location
       * @param {boolean} isCompleted `true` to create a current location
       * @param {object} data The draft data
       * @param {array} positionList The positions for zooming the map
       */
      processActualRouteCurrentLocation: function (isCompleted, data, positionList) {
        var actualRoute = {
          tooltip: this.getRouteTooltip(),
          lineDash: "",
          pos: data.actualRoute.map(function (item) {
            positionList.push([item.longitude, item.latitude]);
            return [item.longitude, item.latitude, 0].join(";");
          }).join(";"),
        };
        data.plannedActualRoutes.push(actualRoute);

        // update current location
        if (!isCompleted) {
          var lastActualEvent = data.actualRoute[data.actualRoute.length - 1];
          data.currentLocations = [deepExtend(lastActualEvent, {
            pos:[lastActualEvent.longitude, lastActualEvent.latitude, 0].join(";"),
          })];
        }
      },

      /**
       * Create a planned route
       * @param {object} data The draft data
       * @param {array} positionList The positions for zooming the map
       */
      processPlannedRoute: function (data, positionList) {
        var plannedRoute = {
          tooltip: this.getRouteTooltip(),
          lineDash: "10;10",
          pos: data.plannedRoute.map(function (item) {
            positionList.push([item.longitude, item.latitude]);
            return [item.longitude, item.latitude, 0].join(";");
          }).join(";"),
        };

        if (data.toConnectPlannedAndActualRoute) {
          var currentLocation = data.currentLocations[0];
          plannedRoute.pos = [currentLocation.pos, plannedRoute.pos].join(";");
        }

        data.plannedActualRoutes.push(plannedRoute);
      },

      /**
       * Create etas for stops
       * @param {boolean} isCompleted `true` to create etas
       * @param {object} data The draft data
       * @param {array} positionList The positions for zooming the map
       */
      processStops: function (isCompleted, data, positionList) {
        data.stopsForVp.forEach(function (stop, index) {
          if (index === 0) {
            stop.isSource = true;
          }
          positionList.push([stop.location.longitude, stop.location.latitude]);
        });

        data.stopsWithETA = isCompleted ? [] : data.stopsForVp.filter(function (stop) {
          return stop.estimatedArrival !== null;
        });
      },

      getRouteTooltip: function () {
        var bindingContext = this.getView().getBindingContext();
        var trackedProcess = bindingContext.getObject({expand: "departureLocation,arrivalLocation"});
        var departureLocation = trackedProcess.departureLocation;
        var arrivalLocation = trackedProcess.arrivalLocation;

        var fromText;
        var toText;
        if (departureLocation) {
          fromText = departureLocation.locationDescription;
        } else {
          fromText = trackedProcess.departureLocationId || this.getText("locationUndefined");
        }
        if (arrivalLocation) {
          toText = arrivalLocation.locationDescription;
        } else {
          toText = trackedProcess.arrivalLocationId || this.getText("locationUndefined");
        }

        return this.getText("routeTooltip", [fromText, toText]);
      },

      zoomRouteAndStopsToAreas: function (positionList) {
        var factor = 0.7;
        if (positionList.length) {
          var map = this.getControlByFragment("trackingTimelineMap", "geoMap");
          map.zoomToAreas(positionList, factor);
        }
      },

      getParentModel: function () {
        return this.getView().getParent().getModel("view");
      },
    });
  }
);
