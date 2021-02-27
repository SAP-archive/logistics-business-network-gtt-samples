sap.ui.define([
  "sap/ui/test/opaQunit",
  "./pages/DeliveryItemPage",
  "./pages/TrackingTimeline",
], function (opaQunit) {
  "use strict";

  QUnit.module("Delivery Item");

  opaQunit("Should display delivery item sections", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "DeliveryItem(guid'c75316ce-a2cd-5f8c-82e4-b4661d3a48e2')",
      timeout: 60,
      autoWait: true,
    });

    // check title in Delivery Item header
    Then.onTheDeliveryItemPage.theTitleShouldDisplayTheNo("187020020 / 10");

    // check title in Delivery Item header
    Then.onTheDeliveryItemPage.theGeneralInfoShouldDisplayTheDangerousGoods("Yes");

    Then.onTheDeliveryItemPage.theReferenceDocumentsTableShouldHaveItems();

    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display the map with routes / spots", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "DeliveryItem(guid'c75316ce-a2cd-5f8c-82e4-b4661d3a48e2')",
      timeout: 60,
      autoWait: true,
    });

    Then.onTheTrackingTimeline.theMapHasLegend();
    Then.onTheTrackingTimeline.theLegendShouldBeExpanded(true);
    When.onTheTrackingTimeline.iPressLegend();
    Then.onTheTrackingTimeline.theLegendShouldBeExpanded(false);

    Then.onTheTrackingTimeline.theMapHasRoutesSpots();
    Then.onTheTrackingTimeline.theMapHasSpotWithErrorType();
    Then.onTheTrackingTimeline.theMapHasNumberOfEventStops(7);

    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display Location Master popover", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "DeliveryItem(guid'c75316ce-a2cd-5f8c-82e4-b4661d3a48e2')",
      timeout: 60,
      autoWait: true,
    });

    // check title in Delivery Item header
    Then.onTheDeliveryItemPage.thePlantShouldDisplayTheDescription("Plant 0001");

    When.onTheDeliveryItemPage.iPressPlantLocationPopover();
    Then.onTheDeliveryItemPage.thePlantLocationPopoverShouldDisplayDetails("QW9CLNT170 / 0001");

    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display number of reference documents", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "DeliveryItem(guid'c75316ce-a2cd-5f8c-82e4-b4661d3a48e2')",
      timeout: 60,
      autoWait: true,
    });

    // check reference documents items lenght
    Then.onTheDeliveryItemPage.theRefDocsTableHasItemsNum(2);

    // Cleanup
    Then.iTeardownMyApp();
  });
  opaQunit("Filter events in timeline", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "DeliveryItem(guid'c75316ce-a2cd-5f8c-82e4-b4661d3a48e2')",
      timeout: 60,
      autoWait: true,
    });

    When.onTheTrackingTimeline.iPressFilterBtn();
    Then.onTheTrackingTimeline.theFilterDialogOpens();

    When.onTheTrackingTimeline.iPressFilterItem();
    When.onTheTrackingTimeline.iPressEventStatus("Late Reported");
    When.onTheTrackingTimeline.iPressFilterByEventStatus();
    Then.onTheTrackingTimeline.theTimelineShouldHaveEvents(3);

    // Cleanup
    Then.iTeardownMyApp();
  });
});
