sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
  "com/sap/gtt/app/sample/pof/util/AsyncUtils",
  "com/sap/gtt/app/sample/pof/controller/mixins/MapHelper.mixin",
], function (BaseController, JSONModel, Constants, ServiceUtils, RestClient, AsyncUtils, MapHelper) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.deliveryItem.TimelineEvents", jQuery.extend({}, MapHelper, {

    initModel: function () {
      this._oTrackingTimeline = new JSONModel();
      this._oTrackingTimeline.setSizeLimit(100);
      this.setModel(this._oTrackingTimeline, "trackingTimeline");

      this.oMapModel = new JSONModel({
        plannedRoutes: [],
        actualRoutes: [],
      });
      this.oMapModel.setSizeLimit(100);
      this.setModel(this.oMapModel, "trackingMap");
    },

    initControls: function () {
      this._oSearchField = this.getControlFromFragment("trackingTimelineEvents", "trackingTimeline-searchField");
      this._oSearchField.setPlaceholder(this.getText("EL_Event_eventMatchKey_LABEL", null, "@i18n"));
      this._oTimeline = this.getControlFromFragment("trackingTimelineEvents", "trackingTimeline");

      this._oMap = this.getControlFromFragment("trackingTimelineMap", "geoMap");
      this.initMap(this._oMap);
      this._initBindingEventHandler();
    },

    /**
     * Click Spot event handler.
     * @param {sap.ui.base.Event} oEvent event object
     */
    onSpotFakeClick: function (oEvent) {
      var oActiveSpot = oEvent.getSource(),
        aAllSpots = oActiveSpot.getParent().getItems(),
        oActiveSpotData = oActiveSpot.getBindingContext("trackingMap").getObject();

      var bAggregatedSpot = aAllSpots.some(function (oSpot) {
        var oSpotData = oSpot.getBindingContext("trackingMap").getObject();
        return oSpotData.latitude === oActiveSpotData.latitude && oSpotData.longitude === oActiveSpotData.longitude && oSpotData.eventId !== oActiveSpotData.eventId;
      });
      if(!bAggregatedSpot) {
        this._openFakeMenu(oActiveSpot, oEvent.getParameter("data").Action.Params.Param);
      }
    },

    /**
     * Open fake menu to show the description.
     * @param {sap.ui.vbm.Spot} oActiveSpot spot control
     * @param {object[]} aClickParams array with coordinates of click
     */
    _openFakeMenu: function (oActiveSpot, aClickParams) {
      var oActiveSpotData = oActiveSpot.getBindingContext("trackingMap").getObject();
      var sDescription = this.getEventTypeLocationDescription(oActiveSpotData);

      oActiveSpot.mClickPos = aClickParams.map(function (oCoordinate) {
        return oCoordinate["#"];
      });

      if (!this._oFakeMenu) {
        this._oFakeMenu = new sap.ui.unified.Menu();
        // this is configuration object to create context menu for the spot
        this._oFakeMenu.vbi_data = {
          VBIName: "DynContextMenu",
          menuRef: "CTM",
          object: "Spot",
          scene: "MainScene",
        };
      }

      this._oFakeMenu.vbi_data.instance = oActiveSpot.getId();
      this._oFakeMenu.removeAllItems();
      this._oFakeMenu.addItem(new sap.ui.unified.MenuItem({
        text: sDescription,
      }));
      oActiveSpot.openContextMenu(this._oFakeMenu);
    },

    /**
     * Change default filter change behaviour for the timeline.
     */
    _initBindingEventHandler: function () {
      this._oTimeline.attachFilterSelectionChange(function (oEvent) {
        var sType = oEvent.getParameter("type"),
          bClear = oEvent.getParameter("clear");
        if (bClear) {
          this._cleanEventMatchKeyFilter();
          return;
        }

        if (sType === sap.suite.ui.commons.TimelineFilterType.Search) {
          this._eventMatchKeyChanged(oEvent);
        }
      }, this);
    },

    /**
     * Remove event match key filter.
     */
    _cleanEventMatchKeyFilter: function () {
      this._oTimeline.setCustomFilterMessage("");
      this._oTimeline.setCustomModelFilter("eventMatchKeyFilter", null);
      this._oSearchField.setValue();
    },

    /**
     * Set empty array to timeline.
     */
    _clearTrackingTimeline: function () {
      this._oTrackingTimeline.setProperty("/", {});
    },

    /**
     * Filter timeline items based on the event match key.
     * @param {sap.ui.base.Event} oEvent search event object
     */
    _eventMatchKeyChanged: function (oEvent) {
      oEvent.preventDefault();
      var sSelectedValue = oEvent.getParameter("searchTerm");
      if (!sSelectedValue) {
        this._cleanEventMatchKeyFilter();
      } else {
        this._oTimeline.setCustomFilterMessage(this.getText("eventMatchKeyLabelTemplate", sSelectedValue));
        this._oTimeline.setCustomModelFilter("eventMatchKeyFilter", new sap.ui.model.Filter({
          path: "eventMatchKey",
          value1: sSelectedValue,
          operator: sap.ui.model.FilterOperator.Contains,
        }));
      }
    },

    refresh: function () {
      this._clearTrackingTimeline();

      this._requestTimelineEvents();
      this._requestMapRoutes();
    },

    /**
     * Request 'timelineEvent' data.
     */
    _requestTimelineEvents: function () {
      var oRestService = ServiceUtils.getDataSource("restService"),
        oBindingContext = this.getView().getBindingContext(),
        sTimelineEventsRequestUrl = ServiceUtils.getUrl(oRestService.uri.concat("/timelineEvent"));

      this._oTimeline.setBusy(true);
      var oTimelineEventsRequest = RestClient.get(sTimelineEventsRequestUrl, {
        params: {
          deliveryItemId: oBindingContext.getProperty("id"),
        },
      });

      oTimelineEventsRequest
        .then(
          function (oData) {
            this._oTrackingTimeline.setProperty("/timelineEvents", oData);
            this.getModel("DeliveryItemDetails").setProperty("/timelineEventsNumber", oData.length);
          }.bind(this),
          function (oError) {
            this.handleServerError(oError);
          }.bind(this));

      AsyncUtils.finally(oTimelineEventsRequest, function () {
        this._oTimeline.setBusy(false);
      }.bind(this));
    },

    /**
     * Request 'routes' data.
     */
    _requestMapRoutes: function () {
      var oRestService = ServiceUtils.getDataSource("restService"),
        oBindingContext = this.getView().getBindingContext(),
        sTimelineEventsRequestUrl = ServiceUtils.getUrl(oRestService.uri.concat("/routes"));

      this._oMap.setBusy(true);
      var oMapRoutesRequest = RestClient.get(sTimelineEventsRequestUrl, {
        params: {
          deliveryItemId: oBindingContext.getProperty("id"),
        },
      });

      oMapRoutesRequest
        .then(function (aRoutesData) {
          this.oMapModel.setProperty("/routes", aRoutesData);
          this.setMapModelData(aRoutesData);
        }.bind(this))
        .catch(function (oError) {
          this.handleServerError(oError);
        }.bind(this))
        .finally(function () {
          this.zoomRouteAndStopsToAreas();
        }.bind(this));

      AsyncUtils.finally(oMapRoutesRequest, function () {
        this._oMap.setBusy(false);
      }.bind(this));
    },

    /**
     * Set data to map model.
     * @param {object[]} aRoutesData routes data array
     */
    setMapModelData: function (aRoutesData) {
      var oRoutes = this.getRoutes(aRoutesData);
      var oEventStops = this.getEventStops(aRoutesData);
      var oCheckResult = this.checkInvalidLocations(aRoutesData);

      this.oMapModel.setProperty("/roughRoutesData", aRoutesData);
      this.oMapModel.setProperty("/stops", oEventStops.stops);
      this.oMapModel.setProperty("/eventStops", oEventStops.eventStops);
      this.oMapModel.setProperty("/currentLocations", this.getCurrentLocations(aRoutesData));
      this.oMapModel.setProperty("/stopsWithETA", oEventStops.stopsWithETA);
      this.oMapModel.setProperty("/actualRoutes", oRoutes.actualRoutes);
      this.oMapModel.setProperty("/plannedRoutes", oRoutes.plannedRoutes);
      this.oMapModel.setProperty("/hasInvalidActualEventLocations", oCheckResult.hasInvalidActualEventLocations);
      this.oMapModel.setProperty("/hasInvalidPlannedEventLocations", oCheckResult.hasInvalidPlannedEventLocations);
    },

    /**
     * Format value of timestam.
     * @param {string} sTimestamp timestamp string
     * @return {string} date time string
     */
    formatTimestamp: function (sTimestamp) {
      if(!sTimestamp) {
        return null;
      }
      var oDateTimeOffset = new sap.ui.model.odata.type.DateTimeOffset();
      return oDateTimeOffset.formatValue(sTimestamp, "string");
    },

    /**
     * Return true | false, if reporting history link should be shown.
     * @param {object} oTimelineEvent timeline event object
     * @return {boolean} true || false
     */
    getHistoryVisibility: function (oTimelineEvent) {
      return this.isActualEvent(oTimelineEvent.eventStatusCode) && !!oTimelineEvent.historicalEvents && !!oTimelineEvent.historicalEvents.length;
    },

    /**
     * Sort stops to show the delayed at the end.
     * @param {object} oPreviousStop stop data
     * @param {object} oNextStop stop data
     * @return {number} key value to define the order
     */
    sortEventStops: function (oPreviousStop, oNextStop) {
      var sPreviousEventType = oPreviousStop.eventStatusCode;
      var sNextEventType = oNextStop.eventStatusCode;
      if (sPreviousEventType === Constants.EVENT_STATUS_CODE.DELAYED) {
        return 1;
      } else if (sNextEventType === Constants.EVENT_STATUS_CODE.DELAYED) {
        return -1;
      } else {
        return 0;
      }
    },

    /**
     * Return true, if event is actual (happened).
     * @param {string} sStatus event status
     * @return {boolean} true || false
     */
    isActualEvent: function (sStatus) {
      return (
        sStatus === Constants.EVENT_STATUS_CODE.REPORTED ||
        sStatus === Constants.EVENT_STATUS_CODE.EARLY_REPORTED ||
        sStatus === Constants.EVENT_STATUS_CODE.LATE_REPORTED ||
        sStatus === Constants.EVENT_STATUS_CODE.REPORTED_ON_TIME ||
        sStatus === Constants.EVENT_STATUS_CODE.UNPLANNED
      );
    },
  }));
});
