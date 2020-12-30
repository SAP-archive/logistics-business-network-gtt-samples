sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/PurchaseOrderListPage",
    "./pages/PurchaseOrderDetailsPage",
    "./pages/PurchaseOrderItemDetailsPage",
  ],
  function (opaQunit) {
    "use strict";

    QUnit.module("Purchase Order List");

    opaQunit("Should open p13n dialog, select Purchase Order No sorting and submit", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });
      // check items in the PO table
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // open personalisation dialog
      When.onThePurchaseOrderListPage.iPressPOPersonalisationBtn();
      Then.onThePurchaseOrderListPage.thePersonalisationDialogAppears();

      When.onThePurchaseOrderListPage.iPressPOPersoSegmentedButton();

      When.onThePurchaseOrderListPage.iPressPOOkPersonalisationBtn();

      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should see the purchase order / purchase order items lists", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });
      // check items in the PO table
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // press POitems filter tab and checkitems there
      When.onThePurchaseOrderListPage.iPressPOItemsTabFilter();
      Then.onThePurchaseOrderListPage.theItemsTableHasEntries();

      // press POitems filter tab and checkitems there
      When.onThePurchaseOrderListPage.iPressPOTabFilter();
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should open and close p13n dialog", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });
      // check items in the PO table
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // open personalisation dialog
      When.onThePurchaseOrderListPage.iPressPOPersonalisationBtn();
      Then.onThePurchaseOrderListPage.thePersonalisationDialogAppears();

      // cancel personalisation dialog
      When.onThePurchaseOrderListPage.iPressPOCancelPersonalisationBtn();
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should navigate from purchase order to its detail", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });
      // check items in the PO table
      Then.onThePurchaseOrderListPage.theTableHasEntries();

      // check press item and navigationto the details
      When.onThePurchaseOrderListPage.iPressOnThePOWithTheID("8ec2f15d-e7a4-5f48-b7b5-da19e853bcf4");
      Then.onThePurchaseOrderDetailsPage.theTitleShouldDisplayTheNo("4500020092");

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should navigate from purchase order item list to its detail", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        timeout: 60,
        autoWait: true,
      });
      // press POitems filter tab and checkitems there
      When.onThePurchaseOrderListPage.iPressPOItemsTabFilter();
      Then.onThePurchaseOrderListPage.theItemsTableHasEntries();

      When.onThePurchaseOrderListPage.iPressMoreInItemsTable();
      Then.onThePurchaseOrderListPage.theItemsTableHasMoreEntries();

      // check press item and navigationto the details
      When.onThePurchaseOrderListPage.iPressOnThePOItemWithTheID("cd509b27-e9b6-548a-a868-a18cfb3ca28e");
      Then.onThePurchaseOrderItemDetailsPage.theTitleShouldDisplayTheNo("4500055229 / 10");

      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
