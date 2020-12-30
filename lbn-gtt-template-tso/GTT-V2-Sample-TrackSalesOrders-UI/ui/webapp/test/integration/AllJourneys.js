sap.ui.define(
  [
    "com/sap/gtt/app/sample/sof/localService/mockserver",
    "sap/ui/test/Opa5",
    "com/sap/gtt/app/sample/sof/test/integration/SalesOrderJourney",
    "com/sap/gtt/app/sample/sof/test/integration/DeliveryItemJourney",
    "com/sap/gtt/app/sample/sof/test/integration/NavigationJourney",
  ],
  function (
    mockserver, Opa5
  ) {
    "use strict";

    // initialize the mock server
    mockserver.init();

    Opa5.extendConfig({
      timeout: 60,
      autoWait: true,
    });
  }
);
