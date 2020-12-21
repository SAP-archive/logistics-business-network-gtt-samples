sap.ui.define([
  "sap/ui/test/Opa5",
  "sap/ui/test/matchers/AggregationFilled",
  "sap/ui/test/matchers/BindingPath",
  "sap/ui/test/actions/Press",
  "sap/ui/test/matchers/PropertyStrictEquals",

],
function (Opa5, AggregationFilled, BindingPath, Press, PropertyStrictEquals) {
  "use strict";

  var sViewName = "PurchaseOrderList";
  var sPurchaseOrderListTableId = /(.*)innerTable$/;
  var sPurchaseOrderItemsTriggerListId = /(.*)innerTableItem-trigger$/;
  var sPurchaseOrderItemsListTableId = /(.*)innerTableItem$/;
  var sPurchaseOrderPersonalisationBtnId = /(.*)purchaseOrderSmartTable-btnPersonalisation$/;
  var sPurchaseOrderPersonalisationDlgId = /(.*)purchaseOrderSmartTable-persoController-P13nDialog$/;
  var sPurchaseOrderPersonalisationCancelBtnId = /(.*)purchaseOrderSmartTable-persoController-P13nDialog-cancel$/;
  var sPurchaseOrderPersonalisationOkBtnId = /(.*)purchaseOrderSmartTable-persoController-P13nDialog-ok$/;

  Opa5.createPageObjects({
    onThePurchaseOrderListPage: {
      actions: {
        iPressPOTabFilter: function () {
          return this.waitFor({
            controlType: "sap.m.IconTabFilter",
            matchers: new PropertyStrictEquals({
              name: "key",
              value: "purchaseOrderTab",
            }),
            actions: new Press(),
            errorMessage: "The purchase order filter tab doesn't exist.",
          });
        },
        iPressPOPersoSegmentedButton: function () {
          return this.waitFor({
            controlType: "sap.m.SegmentedButton",
            success: function (aControl) {
              var oSegmentedButton = aControl[0];
              var oSortButton = oSegmentedButton.getButtons()[1];
              oSortButton.firePress();
              oSegmentedButton.rerender();
              return this.waitFor({
                controlType: "sap.m.ComboBox",
                success: function (aControls) {
                  var oComboBox = aControls[0];
                  oComboBox.setSelectedKey("purchaseOrderNo");
                },
              });
            },
            errorMessage: "The purchase order p13n dialog doesn't exist.",
          });
        },
        iPressPOItemsTabFilter: function () {
          return this.waitFor({
            controlType: "sap.m.IconTabFilter",
            matchers: new PropertyStrictEquals({
              name: "key",
              value: "purchaseOrderItemsTab",
            }),
            actions: new Press(),
            errorMessage: "The purchase order items filter tab doesn't exist.",
          });
        },
        iPressPOPersonalisationBtn: function () {
          return this.waitFor({
            id: sPurchaseOrderPersonalisationBtnId,
            actions: new Press(),
            errorMessage: "The personalisation button doesn't exist.",
          });
        },
        iPressPOCancelPersonalisationBtn: function () {
          return this.waitFor({
            id: sPurchaseOrderPersonalisationCancelBtnId,
            actions: new Press(),
            errorMessage: "The personalisation cancel button doesn't exist.",
          });
        },
        iPressPOOkPersonalisationBtn: function () {
          return this.waitFor({
            id: sPurchaseOrderPersonalisationOkBtnId,
            actions: new Press(),
            errorMessage: "The personalisation OK button doesn't exist.",
          });
        },
        iPressOnThePOWithTheID: function (sId) {
          return this.waitFor({
            controlType: "sap.m.ColumnListItem",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.list",
            matchers:  new BindingPath({
              path: "/PurchaseOrder(guid'" + sId + "')",
            }),
            actions: new Press(),
            success: function (aListItems) {
              // aListItems[0].$().trigger("tap");
            },
            errorMessage: "No list item with the ID " + sId + " was found.",
          });
        },
        iPressOnThePOItemWithTheID: function (sId) {
          return this.waitFor({
            controlType: "sap.m.ColumnListItem",
            viewName: sViewName,
            viewNamespace: "com.sap.gtt.app.sample.pof.view.list",
            matchers:  new BindingPath({
              path: "/PurchaseOrderItem(guid'" + sId + "')",
            }),
            actions: new Press(),
            success: function (aListItems) {
              // aListItems[0].$().trigger("tap");
            },
            errorMessage: "No list item with the ID " + sId + " was found.",
          });
        },
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
        theTableHasEntries: function () {
          return this.waitFor({
            id: sPurchaseOrderListTableId,
            controlType: "sap.m.Table",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The purchase order table has entries.");
            },
            errorMessage: "The table does not have entries.",
          });
        },
        theItemsTableHasEntries: function () {
          return this.waitFor({
            id: sPurchaseOrderItemsListTableId,
            controlType: "sap.m.Table",
            matchers: new AggregationFilled({
              name: "items",
            }),
            success: function () {
              Opa5.assert.ok(true, "The purchase order items table has entries.");
            },
            errorMessage: "The items table does not have entries.",
          });
        },
        thePersonalisationDialogAppears: function () {
          return this.waitFor({
            id: sPurchaseOrderPersonalisationDlgId,
            controlType: "sap.m.P13nDialog",
            matchers: new PropertyStrictEquals({
              name: "title",
              value: "View Settings",
            }),
            success: function () {
              Opa5.assert.ok(true, "The p13n dialog was opened.");
            },
            errorMessage: "The p13n dialog was not opened.",
          });
        },
        theItemsTableHasMoreEntries: function () {
          return this.waitFor({
            id: sPurchaseOrderItemsListTableId,
            matchers: function (oTable) {
              return oTable.getItems().length > 20;
            },
            success: function () {
              Opa5.assert.ok(true, "The purchase order items table can show more than 20 entries.");
            },
            errorMessage: "The table does not have entries.",
          });
        },
      },
    },
  });
}
);
