sap.ui.define([
  "sap/ui/test/Opa5",
  "sap/ui/test/matchers/Properties",
  "sap/ui/test/actions/Press",
  "sap/ui/test/matchers/AggregationFilled",
],
function (Opa5, Properties, Press, AggregationFilled) {
  "use strict";

  var sViewName = "PurchaseOrderDetails";
  var sPurchaseOrderItemsListTableId = /(.*)purchaseOrderDetailsItemsTable--innerTable$/;
  var sPurchaseOrderItemsTriggerListId = /(.*)purchaseOrderDetailsItemsTable--innerTable-trigger$/;
  var sDocumentFlowGraphId = /(.*)PurchaseOrderDetails--documentFlowView--networkGraph$/;

  Opa5.createPageObjects({
    onThePurchaseOrderDetailsPage: {
      actions: {
        iPressMoreInItemsTable: function () {
          return this.waitFor({
            id: sPurchaseOrderItemsTriggerListId,
            controlType: "sap.m.CustomListItem",
            actions: new Press(),
            errorMessage: "The 'More' button doesn't exist.",
          });
        },
      },
      assertions: {
        thePOItemsTableHasEntries: function () {
          return this.waitFor({
            id: sPurchaseOrderItemsListTableId,
            controlType: "sap.m.Table",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The purchase order items table has entries.");
            },
            errorMessage: "The table does not have entries.",
          });
        },
        thePOItemsTableShowsMoreEntries: function () {
          return this.waitFor({
            id: sPurchaseOrderItemsListTableId,
            matchers: function (oTable) {
              return oTable.getItems().length > 5;
            },
            success: function () {
              Opa5.assert.ok(true, "The purchase order items table can show more than 5 entries.");
            },
            errorMessage: "The table does not have entries.",
          });
        },
        theTitleShouldDisplayTheNo: function (sName) {
          return this.waitFor({
            id: "purchaseOrderHeaderTitle",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.order",
            matchers: new Properties({
              text: sName,
            }),
            success: function (oPage) {
              Opa5.assert.ok(true, "The appropriate PO details page is shown.");
            },
            errorMessage: "The PO " + sName + " is not shown",
          });
        },
        thePageHasDocumentFlow: function () {
          return this.waitFor({
            id: sDocumentFlowGraphId,
            matchers: new AggregationFilled({
              name: "nodes",
            }),
            success: function () {
              Opa5.assert.ok(true, "The nodes in document flow are shown.");
            },
            errorMessage: "The nodes in document flow don't exist",
          });
        },
      },
    },
  });
});
