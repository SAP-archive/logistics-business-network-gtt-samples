sap.ui.define(
  [
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/AggregationFilled",
    "sap/ui/test/matchers/BindingPath",
    "sap/ui/test/matchers/PropertyStrictEquals",
    "sap/ui/test/actions/Press",
    "sap/ui/test/actions/EnterText",
  ],
  function (
    Opa5,
    AggregationFilled,
    BindingPath,
    PropertyStrictEquals,
    Press,
    EnterText
  ) {
    "use strict";

    var sShipmentListTableId = /(.*)shipmentList--innerTable$/;
    var sDeliveryNoInputId = /(.*)filterItemControl_BASIC-deliveryId$/;

    Opa5.createPageObjects({
      onTheShipmentListPage: {
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
          iClickShipment: function () {
            return this.waitFor({
              controlType: "sap.m.ColumnListItem",
              matchers: new BindingPath({
                path: "/Shipment(guid'8781028f-e85a-584f-9e95-b878cbf6e92b')",
              }),
              actions: new Press(),
              errorMessage: "Cannot find this shipment",
            });
          },
          iClickLocation: function () {
            return this.waitFor({
              controlType: "sap.m.Link",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Golden Nuts Inc. 2",
              }),
              actions: new Press(),
              errorMessage: "Cannot find a location",
            });
          },
          iSearchDeliveryNo: function (sSearchString) {
            return this.waitFor({
              id: sDeliveryNoInputId,
              controlType: "sap.ui.comp.smartfilterbar.SFBMultiInput",
              actions: new EnterText({
                text: sSearchString,
              }),
              errorMessage: "Cannot find the Input field.",
            });
          },
        },

        assertions: {
          theTableHasEntries: function () {
            return this.waitFor({
              id: sShipmentListTableId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The shipment item table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },
          theLocationHasDetails: function () {
            return this.waitFor({
              controlType: "sap.m.Title",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Golden Nuts Inc. 2",
              }),
              success: function () {
                Opa5.assert.ok(true, "The location has details.");
              },
              errorMessage: "The location does not has details.",
            });
          },
        },
      },
    });
  }
);
