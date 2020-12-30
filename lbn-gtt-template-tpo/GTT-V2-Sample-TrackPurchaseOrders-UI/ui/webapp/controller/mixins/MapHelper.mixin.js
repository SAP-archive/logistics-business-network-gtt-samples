sap.ui.define([
  "sap/base/strings/formatMessage",
  "sap/base/util/deepClone",
  "sap/ui/vbm/SemanticType",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
], function (formatMessage, deepClone, SemanticType, Constants, BaseController, ServiceUtils, RestClient) {
  "use strict";

  var LanguageMappings = {
    "de": "ger",
    "en": "eng",
    "en-US": "eng",
    "es": "spa",
    "fr": "fre",
    "pt": "por",
    "ru": "rus",
    "zh-CN": "chi",
  };

  return {

    initMap: function (oMap) {
      this.oMap = oMap;
      this.keys = {
        "HERE": "key-for-tile-service-of-here",
        "OpenStreet": "39c0ee96f30349beaba4850a6be58b45",
      };

      this.getHereMapKey().then(function () {
        this.setMapConfiguration("HERE");
      }.bind(this));
    },

    getHereMapKey: function () {
      var sHereMapKeyUrl = ServiceUtils.getUrl(ServiceUtils.getDataSource("restService").uri.concat("/hereMapKey"));
      var oRequest = RestClient.get(sHereMapKeyUrl);
      oRequest.then(function (oData) {
        this.keys.HERE = oData.key;
      }.bind(this), function (oError) {
        BaseController.prototype.handleServerError(oError);
      });

      return oRequest;
    },

    setMapConfiguration: function (sProvider) {
      var sProtocol = "https";
      var sHereHost = "1.base.maps.ls.hereapi.com";
      var sAppLanguage = sap.ui.getCore().getConfiguration().getLanguageTag();
      var sTileLanguage = LanguageMappings[sAppLanguage] || "eng";
      var sUrl = formatMessage("{0}://{1}/maptile/2.1/maptile/newest/normal.day/'{LOD}'/'{X}'/'{Y}'/256/png8?apikey={2}&lg={3}", sProtocol, sHereHost, this.keys.HERE, sTileLanguage);

      var oConfiguration = {
        "MapProvider": [{
          "name": "HERE",
          "type": "",
          "description": "",
          "tileX": "256",
          "tileY": "256",
          "maxLOD": "22",
          "copyright": "Copyright © HERE",
          "Source": [{
            "id": "s1",
            "url": sUrl,
          }],
        }],
        "MapLayerStacks": [{
          "name": "DEFAULT",
          "MapLayer": {
            "name": "layer1",
            "refMapProvider": sProvider,
            "opacity": "1.0",
            "colBkgnd": "RGB(255,255,255)",
          },
        }],
      };
      this.oMap.setMapConfiguration(oConfiguration);
      this.oMap.setRefMapLayerStack("DEFAULT");
    },

    getPositionArray: function (oRoute, sSportsPropName) {
      return oRoute[sSportsPropName].map(function (oSpot) {
        return [oSpot.longitude, oSpot.latitude];
      });
    },

    zoomRouteAndStopsToAreas: function () {
      var aRoughRoutesData = this.oMapModel.getProperty("/roughRoutesData") || [];
      var aPositionArray = [];

      for (var iInd = 0; iInd < aRoughRoutesData.length; iInd++) {
        var oRoute = aRoughRoutesData[iInd];
        aPositionArray = this.getPositionArray(oRoute, "plannedSpots").concat(this.getPositionArray(oRoute, "actualSpots"));
      }

      var iFactor = 0.7;
      if (aPositionArray.length) {
        this.byId("trackingTimelineMap--geoMap").zoomToAreas(aPositionArray, iFactor);
      }
    },

    /**
     * Return actual and planned routes
     * @param {object[]} aRoutesData routes data array
     * @return {object} object includes actual and planned routes
     */
    getRoutes: function (aRoutesData) {
      var aActualRoutes = [];
      var aPlannedRoutes = [];

      aRoutesData.forEach(function (oItem) {
        var aActualSpots = deepClone(oItem.actualSpots);
        var aPlannedSpots = this.getPlannedSpots(oItem);
        if (aActualSpots.length) {
          aActualRoutes.push(this.generateRoute(oItem, aActualSpots));
        }
        if (aPlannedSpots.length) {
          aPlannedRoutes.push(this.generateRoute(oItem, aPlannedSpots));
        }
      }.bind(this));

      return {
        actualRoutes: aActualRoutes,
        plannedRoutes: aPlannedRoutes,
      };
    },

    /**
     * Return planned spots (dashed route).
     * In case last reported event (exp.: Delay) refers to planned event, they should be linked via planned route
     *  and planned routes that were before should disappear.
     * In case last reported event is planned event, it should connect to the next stop.
     * In case last reported event is unplanned and it doesn't refer to any planned event, it shouldn't connect to planned spot.
     * @param {object} oItem route data
     * @return {object[]} planned spots array
     */
    getPlannedSpots: function (oItem) {
      var aActualSpots = deepClone(oItem.actualSpots),
        aPlannedSpots = deepClone(oItem.plannedSpots);
      var bPODIsReported = false;

      if (!aActualSpots.length) {
        return aPlannedSpots;
      }

      // If POD is already reported, no need to render planned routes
      bPODIsReported = aActualSpots.some(function (oActualSpot) {
        return oActualSpot.eventType === "POD";
      });
      if (bPODIsReported) {
        return [];
      }

      var oLastReportedEvent = aActualSpots[aActualSpots.length - 1];

      if(!!oLastReportedEvent.plannedEventId && !!aPlannedSpots.length) {
        var oRefPlannedSpot = aPlannedSpots.find(function (oPlannedSpot) {
          return oPlannedSpot.plannedEventId === oLastReportedEvent.plannedEventId;
        });
        if (oRefPlannedSpot && oRefPlannedSpot.eventMatchKey !== oLastReportedEvent.eventMatchKey) {
          var iNextPlannedSpotIndex = aPlannedSpots.indexOf(oRefPlannedSpot);
          aPlannedSpots = aPlannedSpots.slice(iNextPlannedSpotIndex);
        } else {
          var oNextSpot = aPlannedSpots.find(function (oPlannedSpot) {
            return oPlannedSpot.eventMatchKey === oLastReportedEvent.eventMatchKey && oPlannedSpot.eventType === oLastReportedEvent.eventType;
          });
          if (oNextSpot) {
            var iNextSpotIndex = aPlannedSpots.indexOf(oNextSpot);
            aPlannedSpots = aPlannedSpots.slice(iNextSpotIndex + 1);
          }
        }
        aPlannedSpots.unshift(oLastReportedEvent);
      }

      return aPlannedSpots;
    },

    /**
     * Generate and return the route.
     * @param {object} oItem route data
     * @param {object[]} aSpots spots of the route
     * @return {object} route config
     */
    generateRoute: function (oItem, aSpots) {
      return {
        groupId: oItem.groupId,
        lineWidth: Constants.ROUTE_WIDTH.DEFAULT,
        pos: aSpots.map(function (oSpot) {
          return [oSpot.longitude, oSpot.latitude, 0].join(";");
        }).join(";"),
        altKey: oItem.altKey,
        firstPlannedEventId: oItem.firstPlannedEventId,
      };
    },

    getStopIcon: function (sLocationType) {
      switch(sLocationType) {
        case Constants.LOCATION_TYPE_CODE.SHIPPING_POINT:
          return "sap-icon://journey-depart";
        case Constants.LOCATION_TYPE_CODE.CUSTOMER:
          return "sap-icon://visits";
        case Constants.LOCATION_TYPE_CODE.SUPPLIER:
          return "sap-icon://factory";
        case Constants.LOCATION_TYPE_CODE.PLANT:
          return "sap-icon://BusinessSuiteInAppSymbols/icon-warehouse";
        case Constants.LOCATION_TYPE_CODE.LOGISTIC_LOCATION:
          return "sap-icon://functional-location";
        default:
          return "";
      }
    },

    getStopWithETATooltip: function (oItem) {
      var sText = this.getText("stop", [oItem.locationDescription]);
      sText += "\n" + this.getText("estimatedArrivalTime").concat(": ").concat(this.formatTimestamp(oItem.estimatedArrival.estimatedArrivalTime));
      return sText;
    },

    getSpotLabelBorderColor: function (sEventStatusCode) {
      switch (sEventStatusCode) {
        case Constants.EVENT_STATUS_CODE.DELAYED:
        case Constants.EVENT_STATUS_CODE.OVERDUE:
          return "rgb(255, 255, 255)";
      }

      return "rgb(9, 97, 185)";
    },

    getEventTypeLocationDescription: function (oItem) {
      var sDescription = "";
      if (oItem.eventType) {
        var sEventTypeText = this.formatter.eventTypeText.call(this, oItem.eventType);
        sDescription = sEventTypeText;
      }
      var sLocationDescription = oItem.locationDescription;
      if (sLocationDescription) {
        return sDescription.concat(": ").concat(sLocationDescription);
      }

      var sLocationTypeCode = oItem.locationTypeCode;
      if (sLocationTypeCode) {
        var sLocationTypeText = this.formatter.vpLocationTypeText(sLocationTypeCode);
        return sDescription.concat(": ").concat(sLocationTypeText);
      }
      return sDescription;
    },

    getSpotType: function (sEventStatusCode) {
      var sType;
      switch (sEventStatusCode) {
        case Constants.EVENT_STATUS_CODE.DELAYED:
          sType = SemanticType.Error;
          break;
        case Constants.EVENT_STATUS_CODE.OVERDUE:
          sType = SemanticType.Warning;
          break;
        default:
          sType = SemanticType.Default;
          break;
      }
      return sType;
    },

    getStopsDescription: function (aStops) {
      return aStops
        .filter(function (oStop) {
          var oLocation = oStop.location;
          return oLocation && !!oLocation.longitude && !!oLocation.latitude;
        })
        .map(function (oStop) {
          var oLocation = oStop.location;
          return {
            longitude: oLocation.longitude,
            latitude: oLocation.latitude,
            locationTypeCode: oStop.locationTypeCode,
            locationDescription: oLocation.locationDescription,
          };
        });
    },

    getStopsWithETA: function (aPlannedSpots) {
      return aPlannedSpots.filter(function (oSpot) {
        return !!oSpot.estimatedArrival;
      });
    },

    getTempStops: function (aTempStops) {
      return aTempStops.filter(function (oStop) {
        var sEventType = oStop.eventType;
        return sEventType !== Constants.EVENT_TYPE.LOCATION_UPDATE && sEventType !== Constants.EVENT_TYPE.DELAY;
      });
    },

    getEventStops: function (oData) {
      var aStops = [];
      var aStopsWithETA = [];
      var aActualEventStops = [];
      var aPlannedEventStops = [];
      var aTempStops = [];

      for (var iInd = 0; iInd < oData.length; iInd++) {
        var oItem = oData[iInd];
        aStops = this.getStopsDescription(oItem.stops);

        var oExecutionStatusCode = oItem.executionStatusCode;
        if (oExecutionStatusCode !== Constants.EXECUTION_STATUS_CODE.COMPLETED) {
          aStopsWithETA = this.getStopsWithETA(oItem.plannedSpots);
        } else if (oExecutionStatusCode === Constants.EXECUTION_STATUS_CODE.COMPLETED) {
          aTempStops = oItem.actualSpots;
        } else {
          aTempStops = oItem.actualSpots.slice(0, -1);
        }
        aTempStops = this.getTempStops(aTempStops);

        aActualEventStops = aActualEventStops.concat(aTempStops);
        aPlannedEventStops = aPlannedEventStops.concat(oItem.plannedSpots);
      }

      return {
        stops: aStops,
        stopsWithETA: aStopsWithETA,
        eventStops: aActualEventStops.concat(aPlannedEventStops),
      };
    },

    checkInvalidLocations: function (oData) {
      var bInvalidActual = false;
      var bInvalidPlanned = false;

      oData.forEach(function (oItem) {
        if (oItem.groupId) {
          bInvalidActual = bInvalidActual || oItem.actualSpotsValidation.invalid;
          bInvalidPlanned = bInvalidPlanned || !oItem.plannedLocationsValidation.valid;
        }
      });

      return {
        hasInvalidActualEventLocations: bInvalidActual,
        hasInvalidPlannedEventLocations: bInvalidPlanned,
      };
    },

    getCurrentLocations: function (oData) {
      return oData
        .filter(function (oItem) {
          return oItem.actualSpots.length && oItem.executionStatusCode !== Constants.EXECUTION_STATUS_CODE.COMPLETED;
        })
        .map(function (oItem) {
          var oCurrentLocation = oItem.actualSpots.slice(-1)[0];
          oCurrentLocation.transportationModeCode = oItem.transportationModeCode;
          return oCurrentLocation;
        });
    },
  };
});
