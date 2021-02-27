sap.ui.define([
  "sap/ui/test/Opa5",
  "sap/ui/test/matchers/Properties",
  "sap/ui/test/matchers/AggregationFilled",
  "sap/ui/test/matchers/AggregationLengthEquals",
],
function (Opa5, Properties, AggregationFilled, AggregationLengthEquals) {
  "use strict";

  var sViewName = "DeliveryItemDetails";

  Opa5.createPageObjects({
    onTheDeliveryItemPage: {
      actions: {
        iPressPlantLocationPopover: function () {
          return this.waitFor({
            id: "plantLocationPopoverId",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            success: function (oContainer) {
              var oLink = oContainer.getAggregation("_content").getAggregation("items")[1];
              oLink.firePress();
            },
            errorMessage: "The Location popover is not shown",
          });
        },
      },
      assertions: {
        theTitleShouldDisplayTheNo: function (sName) {
          return this.waitFor({
            id: "deliveryItemHeaderTitle",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new Properties({
              text: sName,
            }),
            success: function () {
              Opa5.assert.ok(true, "The appropriate Delivery Item details page is shown.");
            },
            errorMessage: "The Delivery Item " + sName + " is not shown",
          });
        },
        thePlantShouldDisplayTheDescription: function (sName) {
          return this.waitFor({
            id: "plantLocationPopoverId",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new Properties({
              linkText: sName,
            }),
            success: function () {
              Opa5.assert.ok(true, "The appropriate plant location description details page is shown.");
            },
            errorMessage: "The plant location description " + sName + " is not shown",
          });
        },
        thePlantLocationPopoverShouldDisplayDetails: function (sValue) {
          return this.waitFor({
            id: "receivingLocationSourceSystemId",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new Properties({
              text: sValue,
            }),
            success: function () {
              Opa5.assert.ok(true, "The appropriate plant location source id is shown.");
            },
            errorMessage: "The plant location source id " + sValue + " is not shown",
          });
        },
        theGeneralInfoShouldDisplayTheDangerousGoods: function (sName) {
          return this.waitFor({
            fragmentId: "deliveryItemGeneralInfo",
            id: "deliveryItemDangerousGoodsId",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new Properties({
              text: sName,
            }),
            success: function () {
              Opa5.assert.ok(true, "The appropriate Delivery Item general info is shown.");
            },
            errorMessage: "The Delivery Item doesn't display dangerous goods" + sName,
          });
        },
        theReferenceDocumentsTableShouldHaveItems: function () {
          return this.waitFor({
            fragmentId: "referenceBusinessDocumentsTable",
            id: "innerTable",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The Reference Document Items exist.");
            },
            errorMessage: "The Delivery Item doesn't have reference documents items",
          });
        },
        theRefDocsTableHasItemsNum: function (iExpectedNum) {
          return this.waitFor({
            fragmentId: "referenceBusinessDocumentsTable",
            id: "innerTable",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.deliveryItem",
            matchers: new AggregationLengthEquals({
              name: "items",
              length: iExpectedNum,
            }),
            success: function () {
              Opa5.assert.ok(true, "The Reference Document Items has " + iExpectedNum + " items.");
            },
            errorMessage: "The Delivery Item doesn't have reference documents items",
          });
        },
      },
    },
  });
});
