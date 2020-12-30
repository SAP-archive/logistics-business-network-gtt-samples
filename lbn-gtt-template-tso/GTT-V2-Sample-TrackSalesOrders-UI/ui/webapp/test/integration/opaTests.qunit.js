QUnit.config.autostart = false;

sap.ui.getCore().attachInit(function () {
  "use strict";

  sap.ui.require(
    [
      "com/sap/gtt/app/sample/sof/localService/mockserver",
      "com/sap/gtt/app/sample/sof/test/integration/SalesOrderJourney",
      "com/sap/gtt/app/sample/sof/test/integration/DeliveryItemJourney",
    ],
    function (
      mockserver
    ) {
      // set up test service for local testing
      mockserver.init();

      QUnit.start();
    });
});
