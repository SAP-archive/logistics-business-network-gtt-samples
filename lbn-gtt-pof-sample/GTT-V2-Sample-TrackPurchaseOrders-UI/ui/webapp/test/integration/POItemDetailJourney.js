sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/PurchaseOrderItemDetailsPage",
    "./pages/PurchaseOrderDetailsPage",
    "./pages/DocumentFlow",
  ],
  function (opaQunit) {
    "use strict";

    QUnit.module("Purchase Order Item");

    opaQunit("Should display general information", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        hash: "PurchaseOrderItem(guid'1f0be436-6646-5713-95e3-bcdc95a14c9f')",
        timeout: 60,
        autoWait: true,
      });
      // check if sections were rendered
      Then.onThePurchaseOrderItemDetailsPage.theGeneralInfoFormExist();
      Then.onThePurchaseOrderItemDetailsPage.theMilestoneFulfillmentSectionExist();
      Then.onThePurchaseOrderItemDetailsPage.theMilestoneProcessFlowRendered();
      Then.onThePurchaseOrderItemDetailsPage.theDocumentFlowRendered();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should display schedule line table", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        hash: "PurchaseOrderItem(guid'1f0be436-6646-5713-95e3-bcdc95a14c9f')",
        timeout: 60,
        autoWait: true,
      });
      // check if sections were rendered
      Then.onThePurchaseOrderItemDetailsPage.theScheduleLineTableExist();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should display delivery items table", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        hash: "PurchaseOrderItem(guid'1f0be436-6646-5713-95e3-bcdc95a14c9f')",
        timeout: 60,
        autoWait: true,
      });
      // check if sections were rendered
      Then.onThePurchaseOrderItemDetailsPage.theDeliveryItemsTableExist();

      // Cleanup
      Then.iTeardownMyApp();
    });

    opaQunit("Should navigate to Purchase Order detail page - from graph", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/pof",
          async: true,
        },
        hash: "PurchaseOrderItem(guid'1f0be436-6646-5713-95e3-bcdc95a14c9f')",
        timeout: 60,
        autoWait: true,
      });

      Then.onThePurchaseOrderItemDetailsPage.theDocumentFlowRendered();

      When.onTheDocumentFlowView.iPressDocumentFlowNode("4500055266");
      Then.onTheDocumentFlowView.theNavigationPopoverAppears("/nodes/0");

      When.onTheDocumentFlowView.iPressDocumentFlowNavToPOItemDetailButton("/nodes/0");
      Then.onThePurchaseOrderDetailsPage.theTitleShouldDisplayTheNo("4500055266");
      // Cleanup
      Then.iTeardownMyApp();
    });
  }
);
