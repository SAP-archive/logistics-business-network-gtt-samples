sap.ui.define([
  "sap/ui/test/Opa5",
  "sap/ui/test/matchers/Properties",
  "sap/ui/test/matchers/AggregationFilled",
],
function (Opa5, Properties, AggregationFilled) {
  "use strict";

  var sViewName = "POItemDetails";
  var sPOItemHeaderTitleId = /(.*)purchaseOrderItemHeaderTitle$/;
  var sMilestoneProcessFlowId = /(.*)milestoneProcessView--processflow$/;
  var sDocumentFlowGraphId = /(.*)documentFlowView--networkGraph$/;

  Opa5.createPageObjects({
    onThePurchaseOrderItemDetailsPage: {
      actions: {
      },
      assertions: {
        theTitleShouldDisplayTheNo: function (sName) {
          return this.waitFor({
            id: sPOItemHeaderTitleId,
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new Properties({
              text: sName,
            }),
            success: function () {
              Opa5.assert.ok(true, "The appropriate PO Item details page is shown.");
            },
            errorMessage: "The PO Item " + sName + " is not shown",
          });
        },
        theGeneralInfoFormExist: function () {
          return this.waitFor({
            controlType: "sap.uxap.ObjectPageSection",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new Properties({
              title: "General Information",
            }),
            success: function (oPage) {
              Opa5.assert.ok(true, "The General Information section is shown.");
            },
            errorMessage: "The General Information section is hidden.",
          });
        },
        theMilestoneFulfillmentSectionExist: function () {
          return this.waitFor({
            controlType: "sap.uxap.ObjectPageSection",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new Properties({
              title: "Milestone Fulfillment Process",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Milestone Fulfillment Process section is shown.");
            },
            errorMessage: "The Milestone Fulfillment Process section is hidden.",
          });
        },
        theMilestoneProcessFlowRendered: function () {
          return this.waitFor({
            id: sMilestoneProcessFlowId,
            controlType: "sap.suite.ui.commons.ProcessFlow",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new AggregationFilled({
              name: "lanes",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Milestone Fulfillment Process has lanes.");
            },
            errorMessage: "The Milestone Fulfillment Process doesn't have lanes.",
          });
        },
        theDocumentFlowRendered: function () {
          return this.waitFor({
            id: sDocumentFlowGraphId,
            controlType: "sap.suite.ui.commons.networkgraph.Graph",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new AggregationFilled({
              name: "nodes",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Document Flow Graph has nodes.");
            },
            errorMessage: "The Document Flow Graph doesn't have nodes.",
          });
        },
        theScheduleLineTableExist: function () {
          return this.waitFor({
            fragmentId: "purchaseOrderItemDetailsScheduleLinesTable",
            id: "innerTable",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Schedule Line table has items.");
            },
            errorMessage: "The Schedule Line table doesn't have items.",
          });
        },
        theDeliveryItemsTableExist: function () {
          return this.waitFor({
            fragmentId: "deliveryItemsTableFragment",
            id: "innerTable",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.item",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Delivery Items Table has items.");
            },
            errorMessage: "The Delivery Items Table doesn't have items.",
          });
        },
      },
    },
  });
});
