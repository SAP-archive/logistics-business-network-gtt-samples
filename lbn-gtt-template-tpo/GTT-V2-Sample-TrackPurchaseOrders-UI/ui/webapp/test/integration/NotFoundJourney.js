sap.ui.define([
  "sap/ui/test/opaQunit",
  "./pages/NotFoundPage",
  "./pages/PurchaseOrderListPage",
], function (opaQunit) {
  "use strict";

  QUnit.module("Not Found");

  opaQunit("Should display Not Found page", function (Given, When, Then) {
    // Arrangements
    Given.iStartMyUIComponent({
      componentConfig: {
        name: "com/sap/gtt/app/sample/pof",
        async: true,
      },
      hash: "TestUrl",
      timeout: 60,
      autoWait: true,
    });

    Then.onTheNotFoundPage.theTitleShouldDisplayTheNotFound();

    When.onTheNotFoundPage.iPressLink();

    Then.onThePurchaseOrderListPage.theTableHasEntries();

    // Cleanup
    Then.iTeardownMyApp();
  });

});
