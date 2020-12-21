sap.ui.define([
  "sap/ui/test/opaQunit",
  "./pages/PurchaseOrderDetailsPage",
  "./pages/DocumentFlow",
  "./pages/PurchaseOrderItemDetailsPage",
], function (opaQunit) {
  "use strict";

  QUnit.module("Purchase Order Detail");

  opaQunit("Should display items table", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "PurchaseOrder(guid'8ec2f15d-e7a4-5f48-b7b5-da19e853bcf4')",
      timeout: 60,
      autoWait: true,
    });

    // check items table in the PO detail page
    Then.onThePurchaseOrderDetailsPage.thePOItemsTableHasEntries();

    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display more than 5 items in table", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "PurchaseOrder(guid'8ec2f15d-e7a4-5f48-b7b5-da19e853bcf4')",
      timeout: 60,
      autoWait: true,
    });

    // check items table in the PO detail page
    Then.onThePurchaseOrderDetailsPage.thePOItemsTableHasEntries();

    // check if items are loaded after press 'More' button
    When.onThePurchaseOrderDetailsPage.iPressMoreInItemsTable();
    Then.onThePurchaseOrderDetailsPage.thePOItemsTableShowsMoreEntries();
    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display the document flow", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "PurchaseOrder(guid'a3419caf-3597-52b9-89d9-13d63a573767')",
      timeout: 60,
      autoWait: true,
    });

    // check document flow exists
    Then.onThePurchaseOrderDetailsPage.thePageHasDocumentFlow();
    // Cleanup
    Then.iTeardownMyApp();
  });

  opaQunit("Should display navigation popover for Purchase Order Item - in graph", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "PurchaseOrder(guid'18a332e1-140a-5c44-994c-deeafeeeb2b6')",
      timeout: 60,
      autoWait: true,
    });

    // check document flow exists
    Then.onThePurchaseOrderDetailsPage.thePageHasDocumentFlow();

    // check action button appears
    When.onTheDocumentFlowView.iPressDocumentFlowNode("4500055266 / 10");
    Then.onTheDocumentFlowView.theNavigationPopoverAppears("/nodes/1");

    // check navigation to Purchase Order Item detail through action button
    When.onTheDocumentFlowView.iPressDocumentFlowNavToPOItemDetailButton("/nodes/1");
    Then.onThePurchaseOrderItemDetailsPage.theTitleShouldDisplayTheNo("4500055266 / 10");
    // Cleanup
    Then.iTeardownMyApp();
  });
});
