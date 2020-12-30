QUnit.config.autostart = false;

sap.ui.getCore().attachInit(function () {
  "use strict";

  sap.ui.require(
    [
      "com/sap/gtt/app/sample/pof/localService/mockserver",
      "com/sap/gtt/app/sample/pof/test/integration/AllJourneys",
    ],
    function (
      mockserver
    ) {
      // set up test service for local testing
      mockserver.init();

      QUnit.start();
    });
});
