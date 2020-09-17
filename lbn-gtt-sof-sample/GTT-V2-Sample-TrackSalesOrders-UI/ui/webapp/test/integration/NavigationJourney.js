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

    opaTest("Should navigation to the sales order item page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });

      // Actions
      When.onTheSalesOrderListPage.iClickSalesOrder();

      // Assertions
      Then.onTheSalesOrderPage.theTableHasEntries();

      // Actions
      When.onTheSalesOrderPage.iClickSalesOrderItem();

      // Assertions
      Then.onTheSalesOrderItemPage.theDeliveryItemsTableHasEntries();

      // Actions
      When.onTheSalesOrderItemPage.iClickDeliveryItem();

      // Assertions
      Then.onTheDeliveryItemPage.theDeliveryItemHasProcessStatusTag();

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
