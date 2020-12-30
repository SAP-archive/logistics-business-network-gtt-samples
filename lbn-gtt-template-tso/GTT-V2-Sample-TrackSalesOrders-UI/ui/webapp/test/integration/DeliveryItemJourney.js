sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/DeliveryItemPage",
  ],
  function (
    opaTest
  ) {
    "use strict";

    QUnit.module("Delivery Item");

    opaTest("Should see the delivery item page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sof",
          async: true,
        },
        hash: "#/DeliveryItem(guid'1781028f-e85a-584f-9e95-b878cbf6e92g')?salesOrderId=8781028f-e85a-584f-9e95-b878cbf6e92b",
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheDeliveryItemPage.theDeliveryItemHasProcessStatusTag();

      // Actions
      When.onTheDeliveryItemPage.iClickExecutionFlowZoomInButton();
      When.onTheDeliveryItemPage.iClickExecutionFlowZoomOutButton();

      // Actions
      When.onTheDeliveryItemPage.iClickExecutionFlowNode();

      // Assertions
      Then.onTheDeliveryItemPage.iShouldSeeTheHistoricalEventsPopover();

      Then.onTheDeliveryItemPage.theHistoricalEventsListHasItems();

      // Assertions
      Then.onTheDeliveryItemPage.iShouldSeeTheMap();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
