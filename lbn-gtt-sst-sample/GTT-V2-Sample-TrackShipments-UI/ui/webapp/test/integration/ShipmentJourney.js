sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/ShipmentPage",
  ],
  function (
    opaTest
  ) {
    "use strict";

    QUnit.module("Shipment", {
      beforeEach: function () {
      },
      afterEach: function () {
      },
    });

    opaTest("Should see the shipment page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        hash: "#/Shipment(guid'8781028f-e85a-584f-9e95-b878cbf6e92b')",
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheShipmentPage.iShouldSeeTheHeadingTitle();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaTest("Should see the historical events in timeline event", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        hash: "#/Shipment(guid'8781028f-e85a-584f-9e95-b878cbf6e92d')",
        timeout: 60,
        autoWait: true,
      });

      // Actions
      When.onTheShipmentPage.iClickReportingHistoryLink();

      // Assertions
      Then.onTheShipmentPage.iShouldSeeTheReportingHistoryPopover();
      Then.onTheShipmentPage.theHistoricalEventsListHasItems();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaTest("Should see the report events dialog", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        hash: "#/Shipment(guid'8781028f-e85a-584f-9e95-b878cbf6e92b')",
        timeout: 60,
        autoWait: true,
      });

      When.onTheShipmentPage.iPressOnReportButton();
      Then.onTheShipmentPage.iShouldSeeTheReportEventsActionSheet();

      When.onTheShipmentPage.iPressOnReportPlannedEventsButton();
      Then.onTheShipmentPage.iShouldSeeTheReportEventsDialog();

      When.onTheShipmentPage.iPressOnReportButton();

      When.onTheShipmentPage.iClickPlannedEventInputBox()
        .and.iSelectTheFirstPlannedEvent();
      Then.onTheShipmentPage.iShouldSeeTheUserDefinedFieldsForm();

      When.onTheShipmentPage.iReportedBy()
        .and.iPressOnReportButton();

      When.onTheShipmentPage.iPressOnReportButton();
      Then.onTheShipmentPage.iShouldSeeTheReportEventsActionSheet();

      When.onTheShipmentPage.iPressOnReportPlannedEventsButton();
      Then.onTheShipmentPage.iShouldSeeTheReportEventsDialog();
      When.onTheShipmentPage.iPressOnCancelReportEventButton();

      // Cleanup
      Then.iTeardownMyApp();
    });

  }
);
