sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/ShipmentListPage",
    "./pages/ShipmentPage",
  ],
  function (
    opaTest
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

    opaTest("Should navigation to the freight unit page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        hash: "#/FreightUnit(guid'2337b132-c573-5449-b41e-9325e6b18e00')/?itemNo=10",
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheShipmentPage.iShouldSeeTheFreightUnitHeadingTitle();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
