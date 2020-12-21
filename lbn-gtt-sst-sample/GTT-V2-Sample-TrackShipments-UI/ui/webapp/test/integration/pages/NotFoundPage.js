sap.ui.define(
  [
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/AggregationFilled",
    "sap/ui/test/actions/Press",
  ],
  function (
    Opa5,
    AggregationFilled,
    Press
  ) {
    "use strict";

    var linkId = /(.*)notFound--link$/;
    var sShipmentListTableId = /(.*)shipmentList--innerTable$/;

    Opa5.createPageObjects({
      onTheNotFoundPage: {
        actions: {
          iClickWorklistLink: function () {
            return this.waitFor({
              id: linkId,
              controlType: "sap.m.Link",
              actions: new Press(),
              success: function () {
                Opa5.assert.ok(true, "The worklist link is found");
              },
              errorMessage: "Cannot find the worklist link",
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
                Opa5.assert.ok(true, "The shipment table has entries.");
              },
              errorMessage: "The table does not have entries.",
            });
          },
        },
      },
    });
  }
);
