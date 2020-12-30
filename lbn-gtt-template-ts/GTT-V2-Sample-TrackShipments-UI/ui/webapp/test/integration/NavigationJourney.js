sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "com/sap/gtt/app/sample/sst/controller/ShipmentList.controller",
    "./pages/ShipmentListPage",
    "./pages/ShipmentPage",
  ],
  function (
    opaTest,
    ShipmentList
  ) {
    "use strict";

    QUnit.module("Navigation", {
      beforeEach: function () {
      },
      afterEach: function () {
      },
    });

    opaTest("Should navigation to the shipment page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });

      // Actions
      When.onTheShipmentListPage.iClickShipment();

      // Assertions
      Then.onTheShipmentPage.iShouldSeeTheHeadingTitle();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
