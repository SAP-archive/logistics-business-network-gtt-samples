sap.ui.define(
  [
    "./BaseController",
    "sap/base/util/deepClone",
    "sap/ui/core/Fragment",
    "sap/ui/model/json/JSONModel",
    "./MapHelper",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ], function (
    BaseController,
    deepClone,
    Fragment,
    JSONModel,
    MapHelper,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    return BaseController.extend("com.sap.gtt.app.sample.sof.controller.Map", {

      RouteWidth: {
        DEFAULT: 4,
        BOLD: 8,
      },

      initModel: function () {
        var model = new JSONModel();
        this.setModel(model, "map");
        this.setModel(model, "impactAnalysis");
      },

      initControls: function () {
        this.initMap();
      },

      initMap: function () {
        var map = this.byId("geoMap");
        MapHelper.setMapConfiguration(map, "HERE");
      },

      /**
       * Restore model
       */
      resetModel: function () {
        var model = this.getModel("map");
        model.setProperty("/", {
          plannedRoutes: [],
          actualRoutes: [],
          eventStops: [],
          stopsWithETA: [],
          currentLocations: [],
          hasInvalidActualEventLocations: false,
          hasInvalidPlannedEventLocations: false,
        });
      },

      /**
       * Refresh the map view
       */
      refresh: function () {
        this.getView().setBusy(true);

        this.resetModel();
        this.closeSideContent();

        var model = this.getModel("map");
        var bindingContext = this.getView().getBindingContext();
        var deliveryItemId = bindingContext.getProperty("id");
        model.setProperty("/deliveryItemId", deliveryItemId);

        this.updateRoutes();
      },

      updateRoutes: function () {
        var model = this.getModel("map");
        var request = this.getRoutesRequest();
        request.then(function (data) {
          // update parent view
          this.getEventBus().publish("deliveryItem", "updatePlannedArrivalETA", data);

          model.setProperty("/roughRoutesData", data);

          var routes = this.getRoutes(data);
          var eventStops = this.getEventStops(data);
          var checkResult = this.checkInvalidLocations(data);
          model.setProperty("/stops", eventStops.stops);
          model.setProperty("/eventStops", eventStops.eventStops);
          model.setProperty("/currentLocations", this.getCurrentLocations(data));
          model.setProperty("/stopsWithETA", eventStops.stopsWithETA);
          model.setProperty("/actualRoutes", routes.actualRoutes);
          model.setProperty("/plannedRoutes", routes.plannedRoutes);
          model.setProperty("/hasInvalidActualEventLocations", checkResult.hasInvalidActualEventLocations);
          model.setProperty("/hasInvalidPlannedEventLocations", checkResult.hasInvalidPlannedEventLocations);
        }.bind(this), function (error) {
          this.handleServerError(error);
        }.bind(this));
        AsyncUtils.finally(request, function () {
          this.zoomRouteAndStopsToAreas();
          this.getView().setBusy(false);
        }.bind(this));
      },

      zoomRouteAndStopsToAreas: function () {
        var model = this.getModel("map");
        var roughRoutesData = model.getProperty("/roughRoutesData") || [];
        var positionArray = [];

        roughRoutesData.forEach(function (route) {
          route.plannedSpots.forEach(function (spot) {
            positionArray.push([spot.longitude, spot.latitude]);
          });
          route.actualSpots.forEach(function (spot) {
            positionArray.push([spot.longitude, spot.latitude]);
          });
        });

        var factor = 0.7;
        if (positionArray.length) {
          this.byId("geoMap").zoomToAreas(positionArray, factor);
        }
      },

      getRoutesRequest: function () {
        var model = this.getModel("map");
        var jsonService = ServiceUtils.getDataSource("restService");

        var deliveryItemId = model.getProperty("/deliveryItemId");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/routes"));

        return RestClient.get(url, {
          params: {
            deliveryItemId: deliveryItemId,
          },
        });
      },

      getRoutes: function (data) {
        var actualRoutes = [];
        var plannedRoutes = [];
        data.forEach(function (item) {
          var actualSpots = deepClone(item.actualSpots);
          var plannedSpots = deepClone(item.plannedSpots);
          if (actualSpots.length) {
            actualRoutes.push(this.generateRoute(item, actualSpots));
            if (plannedSpots.length) {
              plannedSpots.unshift(actualSpots[actualSpots.length - 1]);
            }
          }
          if (plannedSpots.length) {
            plannedRoutes.push(this.generateRoute(item, plannedSpots));
          }
        }.bind(this));

        return {
          actualRoutes: actualRoutes,
          plannedRoutes: plannedRoutes,
        };
      },

      generateRoute: function (item, spots) {
        return {
          groupId: item.groupId,
          lineWidth: this.RouteWidth.DEFAULT,
          pos: spots.map(function (spot) {
            return [spot.longitude, spot.latitude, 0].join(";");
          }).join(";"),
          altKey: item.altKey,
          firstPlannedEventId: item.firstPlannedEventId,
        };
      },

      getCurrentLocations: function (data) {
        var currentLocations = [];
        data.forEach(function (item) {
          if (item.actualSpots.length && item.executionStatusCode !== "COMPLETED") {
            var currentLocation = item.actualSpots.slice(-1)[0];
            currentLocation.transportationModeCode = item.transportationModeCode;
            currentLocations.push(currentLocation);
          }
        });

        return currentLocations;
      },


      /**
       * Get actual and planned event stops, current location and next stops
       * from the route information
       * @param {object[]} routes The routes information
       * @returns {object} The different stops
       */
      getEventStops: function (routes) {
        var stops = [];
        var stopsWithETA = [];
        var actualEventStops = [];
        var plannedEventStops = [];
        var tempStops = [];

        routes.forEach(function (item) {
          // process all the stops
          item.stops.forEach(function (stop) {
            var location = stop.location;
            if (location && location.longitude !== null && location.latitude !== null) {
              stops.push({
                longitude: location.longitude,
                latitude: location.latitude,
                locationTypeCode: stop.locationTypeCode,
                locationDescription: location.locationDescription,
              });
            }
          });

          // process eta stops
          var executionStatusCode = item.executionStatusCode;
          if (executionStatusCode !== "COMPLETED") {
            item.plannedSpots.forEach(function (spot) {
              if (spot.estimatedArrival) {
                stopsWithETA.push(spot);
              }
            });
          }

          // process actual and planned event stops
          if (executionStatusCode === "COMPLETED") {
            tempStops = item.actualSpots;
          } else {
            tempStops = item.actualSpots.slice(0, -1);
          }
          tempStops = tempStops.filter(function (stop) {
            var eventType = stop.eventType;
            return eventType !== this.formatter.eventType.LOCATION_UPDATE && eventType !== this.formatter.eventType.DELAY;
          }.bind(this));
          actualEventStops = actualEventStops.concat(tempStops);
          plannedEventStops = plannedEventStops.concat(item.plannedSpots);
        }.bind(this));

        return {
          stops: stops,
          stopsWithETA: stopsWithETA,
          eventStops: actualEventStops.concat(plannedEventStops),
        };
      },

      checkInvalidLocations: function (data) {
        var invalidActual = false;
        var invalidPlanned = false;

        data.forEach(function (item) {
          if (item.groupId) {
            invalidActual = invalidActual || item.actualSpotsValidation.invalid;
            invalidPlanned = invalidPlanned || !item.plannedLocationsValidation.valid;
          }
        });

        return {
          hasInvalidActualEventLocations: invalidActual,
          hasInvalidPlannedEventLocations: invalidPlanned,
        };
      },

      /**
       * Show route detail in side content
       * @param {object} oEvent click on route
       */
      showRouteDetail: function (oEvent) {
        this.handleRouteHighlight(oEvent);

        var model = this.getModel("map");
        model.setProperty("/showDetails", false);
        this.showSideContent();

        var currentGroup = model.getProperty("/selectedRoute/groupId");
        var bindingContext = oEvent.getSource().getBindingContext("map");
        var selectedGroup = bindingContext.getProperty("groupId");
        if (currentGroup !== selectedGroup) {
          model.setProperty("/selectedEvents", []);
          model.setProperty("/selectedRoute", bindingContext.getObject());

          // update events on side content
          model.setProperty("/timeLineBusy", true);
          var params = {
            deliveryItemId: model.getProperty("/deliveryItemId"),
          };
          this.updateParametersForEvents(params, bindingContext);
          this.updateEventsOnSideContent(params);
        }
      },

      /**
       * Fetch the events and show them on side content
       * @param {object} params The parameters for GET method
       * @param {string} params.deliveryItemId The delivery item id
       * @param {string} params.eventMatchKey The event match key (group id of the route)
       * @param {[string]} params.plannedEventId The first planned event id
       * @param {[string]} params.altKey The altKey
      */
      updateEventsOnSideContent: function (params) {
        var model = this.getModel("map");
        var jsonService = ServiceUtils.getDataSource("restService");

        var url = ServiceUtils.getUrl(jsonService.uri.concat("/sideContent"));
        var request = RestClient.get(url, {
          params: params,
        });
        request.then(function (data) {
          model.setProperty("/selectedEvents", data);
        }, function (error) {
          this.handleServerError(error);
        }.bind(this));
        AsyncUtils.finally(request, function () {
          model.setProperty("/timeLineBusy", false);
        });
      },

      updateParametersForEvents: function (params, bindingContext) {
        params.eventMatchKey = bindingContext.getProperty("groupId");
        var firstPlannedEventId = bindingContext.getProperty("firstPlannedEventId");
        var altKey = bindingContext.getProperty("altKey");
        if (firstPlannedEventId) {
          params.plannedEventId = firstPlannedEventId;
        }
        if (altKey) {
          params.altKey = altKey;
        }
      },

      showHistoricalReporting: function (oEvent) {
        var model = this.getModel("map");
        model.setProperty("/historicalEvents", []);
        model.setProperty("/showDetails", true);
        this.setControlFocus("backButton", "detailsPanel");

        var bindingContext = oEvent.getSource().getBindingContext("map");
        Fragment.byId(this.createId("detailsPanel"), "panel").bindElement({
          path: bindingContext.getPath(), // "/selectedEvents/0",
          model: "map",
        });
        this.updateHistoricalEvents(model, {
          eventId: bindingContext.getProperty("plannedEventId"),
        });
      },

      initImpactAnalysis: function () {
        var model = this.getModel("impactAnalysis");
        model.setProperty("/isLoaded", false);

        var jsonService = ServiceUtils.getDataSource("restService");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/impactAnalysis/initialNodes"));
        var deliveryItemId = this.getModel("map").getProperty("/deliveryItemId");
        var plannedEventId = model.getProperty("/plannedEventId");

        var request = RestClient.get(url, {
          params: {
            deliveryItemId: deliveryItemId,
            plannedEventId: plannedEventId,
          },
        });

        request.then(function (data) {
          model.setProperty("/nodes", data.nodes);
          model.setProperty("/lines", data.lines.map(function (line) {
            return {
              from: data.nodes[line.from].id,
              to: data.nodes[line.to].id,
              status: line.status,
            };
          }));
          model.setProperty("/isLoaded", true);
        }, function (error) {
          this.handleServerError(error);
          model.setProperty("/isLoaded", true);
        }.bind(this));
      },

      /**
       * Fetch next nodes and refresh impact analysis
       *
       * @param {object} source Event source of clicked node
       * @param {string} id ID (currTpId)
       * @param {string} trackingIdType Tracking ID Type
       */
      updateImpactAnalysis: function (source, id, trackingIdType) {
        if (
          trackingIdType === "SHIPMENT_ORDER" ||
          trackingIdType === "SALES_ORDER" ||
          source.data("childrenLoaded")
        ) {
          return;
        }

        var model = this.getModel("impactAnalysis");
        model.setProperty("/isLoaded", false);

        var jsonService = ServiceUtils.getDataSource("restService");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/impactAnalysis/nextNodes"));
        var request = RestClient.get(url, {
          params: {
            currTpId: id,
            trackingIdType: trackingIdType,
          },
        });

        request.then(function (response) {
          var currentData = model.getData();
          var clickedNodeId = source.getProperty("key");

          model.setProperty("/nodes", currentData.nodes.concat(
            response.nodes.filter(function (nextNode) {
              return !currentData.nodes.some(function (currentNode) {
                return currentNode.id === nextNode.id;
              });
            })
          ));
          model.setProperty("/lines", currentData.lines.concat(
            response.nodes.filter(function (nextNode) {
              return !currentData.lines.some(function (currentLine) {
                return (
                  currentLine.from === clickedNodeId &&
                  currentLine.to === nextNode.id
                );
              });
            }).map(function (nextNode) {
              return {
                from: clickedNodeId,
                to: nextNode.id,
                status: nextNode.status,
              };
            })
          ));
          model.setProperty("/isLoaded", true);
          source.data("childrenLoaded", true);
          source.setCollapsed(false);
        }, function (error) {
          this.handleServerError(error);
          model.setProperty("/isLoaded", true);
        }.bind(this));
      },

      expandPressed: function (source) {
        source.getChildNodes().forEach(function (child) {
          var grandChildNodes = child.getChildNodes();
          if (
            grandChildNodes.length !== 0 &&
            grandChildNodes.every(function (grandChild) {
              return grandChild.isHidden();
            })
          ) {
            child.setCollapsed(true);
          }
        });
      },

      showImpactAnalysis: function (plannedEventId) {
        var model = this.getModel("impactAnalysis");
        model.setProperty("/plannedEventId", plannedEventId);

        if (!this.impactAnalysis) {
          Fragment.load({
            id: this.createId("impactAnalysis"),
            name: "com.sap.gtt.app.sample.sof.view.fragments.ImpactAnalysis",
            controller: this,
          }).then(function (dialog) {
            this.impactAnalysis = dialog;
            this.getView().addDependent(this.impactAnalysis);
            this.impactAnalysis.open();
          }.bind(this));
        } else {
          this.impactAnalysis.open();
        }
      },

      hideImpactAnalysis: function () {
        if (this.impactAnalysis) {
          this.impactAnalysis.close();
        }
        var model = this.getModel("impactAnalysis");
        model.setProperty("/nodes", []);
        model.setProperty("/lines", []);
      },

      /**
       * Triggered when pressing back button on details panel
       */
      showEventsPanel: function () {
        var model = this.getModel("map");
        model.setProperty("/showDetails", false);
        this.setControlFocus("closeButton", "eventsPanel");
      },

      closeSideContent: function () {
        var dynamicSideContent = this.byId("dynamicSideContent");
        dynamicSideContent.setShowSideContent(false);
        dynamicSideContent.fireBreakpointChanged({currentBreakpoint: dynamicSideContent.getCurrentBreakpoint()});
        this.cancelRouteHighlight();
      },

      /**
       * Set focus for the control
       * @param {string} controlId The control's id
       * @param {[string]} fragmentId The fragment's id
       */
      setControlFocus: function (controlId, fragmentId) {
        if (fragmentId) {
          setTimeout(function () {
            Fragment.byId(this.createId(fragmentId), controlId).focus();
          }.bind(this), 0);
        } else {
          setTimeout(function () {
            this.byId(controlId).focus();
          }.bind(this), 0);
        }
      },

      /**
       * Handle the highlight of the route
       *
       * @param {sap.ui.base.Event} oEvent The UI5 event
       */
      handleRouteHighlight: function (oEvent) {
        this.cancelRouteHighlight();

        // highlight process route
        this.selectedRoute = oEvent.getSource();
        this.highlightRoute();
      },

      /**
       * Highlight the route if a user select a location or route
       */
      highlightRoute: function () {
        this.toggleHighlightRouteWidth(this.RouteWidth.BOLD);
      },

      /**
       * Cancel the route highlight behavior
       */
      cancelRouteHighlight: function () {
        if (this.selectedRoute) {
          this.toggleHighlightRouteWidth(this.RouteWidth.NORMAL);
        }
      },

      toggleHighlightRouteWidth: function (width) {
        var model = this.getModel("map");
        var bindingContext = this.selectedRoute.getBindingContext("map");
        var groupId = bindingContext.getProperty("groupId");
        var plannedRoutePath = this.getRouteBindingPath(groupId, "planned");
        var actualRoutePath = this.getRouteBindingPath(groupId, "actual");
        if (plannedRoutePath) {
          model.setProperty("/plannedRoutes" + plannedRoutePath + "/lineWidth", width);
        }
        if (actualRoutePath) {
          model.setProperty("/actualRoutes" + actualRoutePath + "/lineWidth", width);
        }
      },

      /**
       * Get route binding path according to groupId
       *
       * @param {string} groupId The groupId of the route
       * @param {string} type 'planned' or 'actual'
       * @returns {string} The binding path '/actualRoutes/0' or '/plannedRoutes/0'
       */
      getRouteBindingPath: function (groupId, type) {
        var model = this.getModel("map");
        var routes = [];
        var path = null;
        if (type === "planned") {
          routes = model.getProperty("/plannedRoutes");
        } else {
          routes = model.getProperty("/actualRoutes");
        }
        routes.forEach(function (route, index) {
          if (route.groupId === groupId) {
            path = "/".concat(index);
          }
        });

        return path;
      },

      /**
       * Set visibilities of components
       */
      showSideContent: function () {
        var dynamicSideContent = this.byId("dynamicSideContent");
        dynamicSideContent.setShowSideContent(true);
        dynamicSideContent.fireBreakpointChanged({currentBreakpoint: dynamicSideContent.getCurrentBreakpoint()});
        this.setControlFocus("closeButton", "eventsPanel");
      },

      /**
       * Toggle visiblilities of side content when window size changed
       * @param {sap.ui.base.Event} oEvent UI5 event object
       */
      onBreakPointChanged: function (oEvent) {
        var currentBreakpoint = oEvent.getParameter("currentBreakpoint");
        var dynamicSideContent = this.byId("dynamicSideContent");
        var isSideContentShown = dynamicSideContent.getShowSideContent();
        if ((currentBreakpoint === "M" || currentBreakpoint === "S") && isSideContentShown) {
          dynamicSideContent.setShowMainContent(false);
          dynamicSideContent.setShowSideContent(true);
        } else {
          dynamicSideContent.setShowMainContent(true);
        }
      },

    });
  }
);
