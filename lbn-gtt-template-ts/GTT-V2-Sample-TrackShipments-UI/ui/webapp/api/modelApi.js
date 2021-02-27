sap.ui.define([
  "../util/Api",
  "../util/RestClient",
], function (Api, RestClient) {
  "use strict";

  var modelApi = Api.define("restService", {

    /**
     * Get model fields by event type
     *
     * @param {string} model Model name
     * @param {string} eventType Event type
     * @returns {Promise<EventTypeFieldsResult>} Fields
     */
    getFieldsByEventType: function (model, eventType) {
      return RestClient.get(
        this.createUrl("/models/" + model + "/eventTypes/" + eventType + "/fields")
      );
    },

    /**
     * Get unplanned events
     *
     * @param {string} model Model name
     * @returns {Promise<UnplannedEventInfo[]>} Unplanned events
     */
    getUnplannedEvents: function (model) {
      return RestClient.get(
        this.createUrl("/models/" + model + "/unplannedEvents")
      );
    },

  });

  return modelApi;
});
