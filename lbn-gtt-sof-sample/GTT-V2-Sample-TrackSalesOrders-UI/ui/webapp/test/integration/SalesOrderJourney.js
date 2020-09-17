sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/SalesOrderPage",
  ],
  function (
    opaTest
  ) {
    "use strict";

    QUnit.module("Sales Order");

    opaTest("Should see the sales order page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sof",
          async: true,
        },
        hash: "#/SalesOrder(guid'8781028f-e85a-584f-9e95-b878cbf6e92b')",
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheSalesOrderPage.theTableHasEntries();

      Then.onTheSalesOrderPage.iShouldSeeTheDelayedGenericTag();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaTest("Should see the sales order list page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheSalesOrderListPage.theSalesOrdersTableHasEntries();

      // Actions
      When.onTheSalesOrderListPage.iSearchTheMaterialNo("abc")
        .and.iPressOnGoButton();

      // Assertions
      Then.onTheSalesOrderListPage.theSalesOrdersTableHasEntries();

      // Actions
      When.onTheSalesOrderListPage.iSearchTheMaterialDescription("abc")
        .and.iPressOnGoButton();

      // Assertions
      Then.onTheSalesOrderListPage.theSalesOrdersTableHasEntries();

      // Actions
      When.onTheSalesOrderListPage.iSearchShipmentNo("abc")
        .and.iPressOnGoButton();

      // Assertions
      Then.onTheSalesOrderListPage.theSalesOrdersTableHasEntries();

      // Actions
      When.onTheSalesOrderListPage.iClickIsDelayedDownButton()
        .and.iClickIsDelayedOption("Yes")
        .and.iPressOnGoButton();

      // Assertions
      Then.onTheSalesOrderListPage.theSalesOrdersTableHasOneEntry();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaTest("Should see the sales order item page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sof",
          async: true,
        },
        hash: "#/SalesOrderItem(guid'8781028f-e85a-584f-9e95-b878cbf6e92c')?salesOrderId=8781028f-e85a-584f-9e95-b878cbf6e92b",
        timeout: 60,
        autoWait: true,
      });

      // Assertions
      Then.onTheSalesOrderItemPage.theDeliveryItemsTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
