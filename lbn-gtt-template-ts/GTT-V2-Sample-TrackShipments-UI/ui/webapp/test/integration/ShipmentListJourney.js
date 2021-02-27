sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/ShipmentListPage",
  ],
  function (
    opaTest
  ) {
    "use strict";

    QUnit.module("Shipment List");

    opaTest("Should see the shipment list", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheShipmentListPage.theTableHasEntries();

      // Actions
      When.onTheShipmentListPage.iClickLocation();

      // Assertions
      Then.onTheShipmentListPage.theLocationHasDetails();

      // Actions
      When.onTheShipmentListPage.iSearchDeliveryNo("abc")
        .and.iPressOnGoButton();

      // Assertions
      Then.onTheShipmentListPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaTest("Should filter the shipment list", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
          componentData: {
            startupParameters: {
              shipmentNo: "No.123",
            },
          },
        },
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheShipmentListPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
