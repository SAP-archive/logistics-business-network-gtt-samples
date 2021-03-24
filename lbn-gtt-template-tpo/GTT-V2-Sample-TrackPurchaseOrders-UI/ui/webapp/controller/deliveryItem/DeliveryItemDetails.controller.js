sap.ui.define([
  "com/sap/gtt/app/sample/pof/controller/BaseController",
  "sap/ui/model/json/JSONModel",
  "sap/ui/core/format/DateFormat",
  "com/sap/gtt/app/sample/pof/util/ServiceUtils",
  "com/sap/gtt/app/sample/pof/util/RestClient",
], function (BaseController,JSONModel, DateFormat,  ServiceUtils, RestClient) {
  "use strict";

  return BaseController.extend("com.sap.gtt.app.sample.pof.controller.deliveryItem.DeliveryItemDetails", {
    routeName: "DeliveryItemDetails",
    sDeliveryItemIdPath: "/deliveryItemDetailsId",
    sReferenceDocumentsLoadedPath: "/isReferenceDocumentsLoaded",

    initModel: function () {
      var oModel = new JSONModel({
        lastUpdatedTimeText: "",
      });
      this.setModel(oModel, this.routeName);
      this.initGenericTags(["processStatusGenericTag","executionStatusGenericTag"]);
      this.registerEvents(this.routeName);
    },

    routePatternMatched: function (oEvent) {
      var oModel = this.getModel(this.routeName);
      var oArgs = oEvent.getParameter("arguments");
      var sId = oArgs.id;
      var aUrlParams = oArgs["?params"];
      oModel.setProperty(this.sDeliveryItemIdPath, sId);
      oModel.setProperty("/urlParams", aUrlParams);
      oModel.setProperty("/timelineEventsNumber", 0);

      this.bindDeliveryItemToView();
    },

    bindDeliveryItemToView: function () {
      var oODataModel = this.getModel();
      oODataModel.metadataLoaded()
        .then(function () {
          var sEntitySetKey = oODataModel.createKey("InboundDeliveryItem", {
            id: this.getModel(this.routeName).getProperty(this.sDeliveryItemIdPath),
          });
          this.bindView(sEntitySetKey, {
            expand: "incoterms,toSupplierLocation,toPlantLocation,plantLocationType,supplierLocationType",
          });
          this.setLastUpdatedTime();
        }.bind(this));
    },

    updateView: function () {
      var oBindingContext = this.getView().getBindingContext(),
        sPOItemNo = oBindingContext.getProperty("id");

      this.getModel(this.routeName).setProperty(this.sDeliveryItemIdPath, sPOItemNo);

      this._refreshTrackingTimeline();
      this._refreshFulfillmentStatus();
      this._refreshReferenceDocuments();
    },

    /**
     * Fire request to get fresh timeline events and map data.
     */
    _refreshTrackingTimeline: function () {
      var oTrackingTimelineController = this.byId("trackingTimelineView").getController();
      oTrackingTimelineController.refresh();
    },

    /**
     * Fire request to get fresh fulfillmentStatus data.
     */
    _refreshFulfillmentStatus: function () {
      var oModel = this.getModel(this.routeName);
      var oJsonService = ServiceUtils.getDataSource("restService");
      var sDeliveryItemId = oModel.getProperty(this.sDeliveryItemIdPath);

      var sUrl = ServiceUtils.getUrl(oJsonService.uri + "/fulfillmentStatus");
      var oRequest = RestClient.get(sUrl, {
        params: {
          inboundDeliveryItemId: sDeliveryItemId,
        },
      });

      oRequest.then(function (aData) {
        oModel.setProperty("/actualEventsCount", aData.length);
        oModel.setProperty("/fulfillmentStatusSet", this.groupEventsByStatus(aData));
      }.bind(this),
      function (oError) {
        this.handleServerError(oError);
      }.bind(this));
    },

    /**
     * Fire request to get fresh reference document data.
     */
    _refreshReferenceDocuments: function () {
      var oModel = this.getModel(this.routeName);
      oModel.setProperty(this.sReferenceDocumentsLoadedPath, false);

      var oJsonService = ServiceUtils.getDataSource("restService");
      var sDeliveryItemId = oModel.getProperty(this.sDeliveryItemIdPath);
      var sUrl = ServiceUtils.getUrl(oJsonService.uri + "/carrierRefDocuments");
      var oRequest = RestClient.get(sUrl, {
        params: {
          deliveryItemId: sDeliveryItemId,
        },
      });

      oRequest.then(function (aData) {
        oModel.setProperty("/referenceDocuments", this.getFilteredReferenceDocs(aData));
        oModel.setProperty(this.sReferenceDocumentsLoadedPath, true);
      }.bind(this), function (oError) {
        this.handleServerError(oError);
        oModel.setProperty(this.sReferenceDocumentsLoadedPath, true);
      }.bind(this));
    },

    /**
     * Return the array of Reference Documents, that have ID.
     * @param {Object[]} aReferenceDocs Reference Documents array
     * @return {Object[]} filtered Reference Documents array
     */
    getFilteredReferenceDocs: function (aReferenceDocs) {
      return aReferenceDocs.filter(function (oDoc) {
        return !!oDoc.docId;
      });
    },

    /**
     * Group the events for chart on the page header
     *
     * @param {object[]} aEvents The restructured events to be grouped
     * @returns {object[]} The grouped events
     */
    groupEventsByStatus: function (aEvents) {
      var oGroupNo = {
        DELAYED: 0,
        OVERDUE: 1,
        REPORTED: 2,
        PLANNED: 3,
      };
      return aEvents.sort(function (a, b) {
        return oGroupNo[a.eventStatus_code] - oGroupNo[b.eventStatus_code];
      });
    },

    onRefreshBtnPressed: function (oEvent) {
      // refresh view binding
      this.getView().getElementBinding().refresh(true);
      this.updateView();

      this.setLastUpdatedTime();
    },

    setLastUpdatedTime: function () {
      var oMediumDateFormat = DateFormat.getDateTimeInstance({
        style: "medium",
        UTC: false,
      });
      var dLastUpdated = oMediumDateFormat.format(new Date());

      this.getModel(this.routeName).setProperty("/lastUpdatedTime", dLastUpdated);
      this.getModel(this.routeName).setProperty("/lastUpdatedTimeText", this.getText("lastRefreshedAtTimeLabel", dLastUpdated));
    },
  });
});
