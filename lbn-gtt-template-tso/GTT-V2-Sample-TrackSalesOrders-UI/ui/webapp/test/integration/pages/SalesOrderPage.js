sap.ui.define(
  [
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/AggregationFilled",
    "sap/ui/test/matchers/AggregationEmpty",
    "sap/ui/test/matchers/BindingPath",
    "sap/ui/test/actions/Press",
    "sap/ui/test/matchers/PropertyStrictEquals",
    "sap/ui/test/actions/EnterText",
  ],
  function (
    Opa5,
    AggregationFilled,
    AggregationEmpty,
    BindingPath,
    Press,
    PropertyStrictEquals,
    EnterText
  ) {
    "use strict";

    var sSalesOrderItemsTableId = /(.*)salesOrderItemsView--salesOrderItemsTable$/;
    var sSalesOrderListTableId = /(.*)salesOrderList--innerTable$/;
    var sDeliveryItemsTabldId = /(.*)deliveryItemsView--deliveryItemsTable$/;
    var sIsDelayedComboBoxId = /(.*)customFilterFragment--isDelayedComboBox-arrow$/;
    var sMaterialNoInputId = /(.*)filterItemControl_BASIC-vMaterialNo$/;
    var sMaterialDescriptionInputId = /(.*)filterItemControl_BASIC-vMaterialDescription$/;
    var sShipmentNoInputId = /(.*)customFilterFragment--shipmentNoInput$/;
    var sSalesOrderIsDelayedGenericTagId = /(.*)salesOrder--isDelayedGenericTag$/;

    Opa5.createPageObjects({
      onTheSalesOrderListPage: {
        actions: {
          iPressOnGoButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Go",
              }),
              actions: new Press(),
              errorMessage: "Does not have a Go Button",
            });
          },

          iClickSalesOrder: function () {
            return this.waitFor({
              controlType: "sap.m.ColumnListItem",
              matchers: new BindingPath({
                path: "/SalesOrder(guid'8781028f-e85a-584f-9e95-b878cbf6e92b')",
              }),
              actions: new Press(),
              errorMessage: "Cannot find this sales order",
            });
          },

          iClickIsDelayedDownButton: function () {
            return this.waitFor({
              id: sIsDelayedComboBoxId,
              actions: new Press(),
              errorMessage: "Cannot find the button",
            });
          },

          iClickIsDelayedOption: function (sSearchString) {
            return this.waitFor({
              controlType: "sap.m.StandardListItem",
              matchers: new PropertyStrictEquals({
                name: "title",
                value: sSearchString,
              }),
              actions: new Press(),
              errorMessage: "cannot find this listItem",
            });
          },

          iSearchTheMaterialNo: function (sSearchString) {
            return this.waitFor({
              id: sMaterialNoInputId,
              controlType: "sap.ui.comp.smartfilterbar.SFBMultiInput",
              actions: new EnterText({
                text: sSearchString,
              }),
              errorMessage: "Cannot find the Input field.",
            });
          },

          iSearchTheMaterialDescription: function (sSearchString) {
            return this.waitFor({
              id: sMaterialDescriptionInputId,
              controlType: "sap.ui.comp.smartfilterbar.SFBMultiInput",
              actions: new EnterText({
                text: sSearchString,
              }),
              errorMessage: "Cannot find the Input field.",
            });
          },

          iSearchShipmentNo: function (sSearchString) {
            return this.waitFor({
              id: sShipmentNoInputId,
              controlType: "sap.m.Input",
              actions: new EnterText({
                text: sSearchString,
              }),
              errorMessage: "Cannot find the Input field.",
            });
          },
        },

        assertions: {
          theSalesOrdersTableHasEntries: function () {
            return this.waitFor({
              id: sSalesOrderListTableId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },

          theSalesOrdersTableHasOneEntry: function () {
            return this.waitFor({
              id: sSalesOrderListTableId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function (data) {
                var isDelayed = data[0].getItems()[0].getBindingContext().getProperty("isDelayed");
                Opa5.assert.ok(isDelayed === true, "The table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },

          theSalesOrdersTableHasNoEntries: function () {
            return this.waitFor({
              id: sSalesOrderListTableId,
              matchers: new AggregationEmpty({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },
        },
      },

      onTheSalesOrderPage: {
        actions: {
          iClickSalesOrderItem: function () {
            return this.waitFor({
              controlType: "sap.m.ColumnListItem",
              matchers: new BindingPath({
                path: "/SalesOrderItem(guid'8781028f-e85a-584f-9e95-b878cbf6e92c')",
              }),
              actions: new Press(),
              errorMessage: "Cannot find this sales order item",
            });
          },
        },

        assertions: {
          theTableHasEntries: function () {
            return this.waitFor({
              id: sSalesOrderItemsTableId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The sales order item table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },

          iShouldSeeTheDelayedGenericTag: function () {
            return this.waitFor({
              id: sSalesOrderIsDelayedGenericTagId,
              controlType: "sap.m.GenericTag",
              success: function () {
                Opa5.assert.ok(true, "The isDelayed generic tag is shown");
              },
              errorMessage: "Cannot find the isDelayed generic tag",
            });
          },
        },
      },

      onTheSalesOrderItemPage: {
        actions: {
          iClickDeliveryItem: function () {
            return this.waitFor({
              controlType: "sap.m.ColumnListItem",
              matchers: new BindingPath({
                path: "/DeliveryItem(guid'1781028f-e85a-584f-9e95-b878cbf6e92g')",
              }),
              actions: new Press(),
              errorMessage: "Cannot find this delivery item",
            });
          },
        },

        assertions: {
          theDeliveryItemsTableHasEntries: function () {
            return this.waitFor({
              id: sDeliveryItemsTabldId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },
        },
      },
    });
  }
);
