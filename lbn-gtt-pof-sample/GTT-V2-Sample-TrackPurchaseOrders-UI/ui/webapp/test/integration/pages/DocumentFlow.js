sap.ui.define([
  "sap/ui/test/Opa5",
  "sap/ui/test/matchers/BindingPath",
  "sap/ui/test/matchers/Properties",
  "sap/ui/test/actions/Press",
],
function (Opa5, BindingPath, Properties, Press) {
  "use strict";

  Opa5.createPageObjects({
    onTheDocumentFlowView: {
      actions: {
        iPressDocumentFlowNode: function (sTitle) {
          return this.waitFor({
            controlType: "sap.suite.ui.commons.networkgraph.Node",
            matchers:  new Properties({
              title: sTitle,
            }),
            actions: new Press(),
            errorMessage: "The Purchase Order Item node doesn't exist",
          });
        },
        iPressDocumentFlowNavToPOItemDetailButton: function (sBindingPath) {
          return this.waitFor({
            controlType: "sap.suite.ui.commons.networkgraph.ActionButton",
            matchers: new Properties({
              icon: "sap-icon://attachment",
              enabled: true,
            }),
            actions: new Press(),
            errorMessage: "The 'Action' button doesn't exist.",
          });
        },
      },
      assertions: {
        theNavigationPopoverAppears: function (sBindingPath) {
          return this.waitFor({
            controlType: "sap.suite.ui.commons.networkgraph.ActionButton",
            matchers: new BindingPath({
              modelName: "graph",
              path: sBindingPath,
            }),
            success: function () {
              Opa5.assert.ok(true, "The navigation popover appears, once node is selected.");
            },
            errorMessage: "The navigation popover doesn't appear.",
          });
        },
      },
    },
  });
});
