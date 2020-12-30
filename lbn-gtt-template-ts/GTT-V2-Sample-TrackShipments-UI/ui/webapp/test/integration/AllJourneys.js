sap.ui.define(
  [
    "com/sap/gtt/app/sample/sst/localService/mockserver",
    "sap/ui/test/Opa5",
    "com/sap/gtt/app/sample/sst/test/integration/ShipmentListJourney",
    "com/sap/gtt/app/sample/sst/test/integration/ShipmentJourney",
    "com/sap/gtt/app/sample/sst/test/integration/NavigationJourney",
    "com/sap/gtt/app/sample/sst/test/integration/NotFoundJourney",
  ],
  function (
    mockserver,
    Opa5
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
