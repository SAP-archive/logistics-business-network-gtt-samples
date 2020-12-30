sap.ui.define([
  "com/sap/gtt/app/sample/pof/localService/mockserver",
  "sap/ui/test/Opa5",
  "com/sap/gtt/app/sample/pof/test/integration/PurchaseOrderListJourney",
  "com/sap/gtt/app/sample/pof/test/integration/PurchaseOrderDetailJourney",
  "com/sap/gtt/app/sample/pof/test/integration/POItemDetailJourney",
  "com/sap/gtt/app/sample/pof/test/integration/DeliveryItemJourney",
  "com/sap/gtt/app/sample/pof/test/integration/NotFoundJourney",
], function (mockserver, Opa5) {
  "use strict";

  // initialize the mock server
  mockserver.init();

  Opa5.extendConfig({
    timeout: 60,
    autoWait: true,
  });
});
