sap.ui.define([
  "sap/ui/test/Opa5",
],
function (Opa5) {
  "use strict";

  var sViewName = "NotFound";

  Opa5.createPageObjects({
    onTheNotFoundPage: {
      actions: {
        iPressLink: function () {
          return this.waitFor({
            controlType: "sap.m.Link",
            success: function (aLink) {
              var oLink = aLink[0];
              oLink.firePress();
            },
            errorMessage: "The link doesn't exist.",
          });
        },
      },
      assertions: {
        theTitleShouldDisplayTheNotFound: function () {
          return this.waitFor({
            id: "link",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view",
            success: function () {
              Opa5.assert.ok(true, "The appropriate message page is shown.");
            },
            errorMessage: "The Message Page with title is not shown",
          });
        },
      },
    },
  });
});
