sap.ui.define([
  "../util/Api",
  "../util/RestClient",
], function (Api, RestClient) {
  "use strict";

  var eventApi = Api.define("restService", {

    /**
     * Get code list by its name
     *
     * @param {string} name Code list name
     * @returns {Promise<CodeInfo[]>} Code list
     */
    getCodeListByName: function (name) {
      return RestClient.get(
        this.createUrl("/events/codeLists/" + name)
      );
    },

    /**
     * Report event with payload
     *
     * @param {string} eventType Event type
     * @param {object} payload Paylaod of reporting
     * @returns {Promise<any>} Result of reporting
     */
    report: function (eventType, payload) {
      return RestClient.post(
        this.createUrl("/events/" + eventType),
        payload
      );
    },

  });

  return eventApi;
});
