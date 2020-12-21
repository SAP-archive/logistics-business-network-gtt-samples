sap.ui.define(function () {
  "use strict";

  var NetworkGraph = {};

  /**
   * Enum type for the event type
   *
   * @readonly
   * @enum {string}
  */
  NetworkGraph.Group = {
    SALES_ORDER: 1,
    SALES_ORDER_ITEM: 2,
    DELIVERY_ITEM: 3,
    DELIVERY: 4,
    SHIPMENT: 5,
    RESOURCE: 6,
  };

  return Object.freeze(NetworkGraph);
});
