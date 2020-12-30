sap.ui.define(
  [
    "sap/ui/test/opaQunit",
    "./pages/NotFoundPage",
  ],
  function (
    opaTest
  ) {
    "use strict";

    QUnit.module("Not Found");

    opaTest("Should see not found page", function (Given, When, Then) {
      // Arrangements
      Given.iStartMyUIComponent({
        componentConfig: {
          name: "com/sap/gtt/app/sample/sst",
          async: true,
        },
        hash: "#/abc",
        timeout: 60,
        autoWait: true,
      });

      // Actions
      When.onTheNotFoundPage.iClickWorklistLink();

      // Assertions
      Then.onTheNotFoundPage.theTableHasEntries();

      // Cleanup
      Then.iTeardownMyApp();
    });

  }
);
