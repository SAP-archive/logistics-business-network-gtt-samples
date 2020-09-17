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

    var sProcessStatusTagId = /(.*)deliveryItem--processStatusGenericTag$/;
    var sHistoricalEventsPopoverId = /(.*)eventsPopover--popover-popover$/;
    var sHistoricalEventsListId = /(.*)eventsPopover--list$/;
    var sGeoMapId = /(.*)deliveryItem--mapView--geoMap$/;

    Opa5.createPageObjects({
      onTheDeliveryItemPage: {
        actions: {
          iClickExecutionFlowZoomInButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "icon",
                value: "sap-icon://zoom-in",
              }),
              actions: new Press(),
              errorMessage: "Cannot find the Zoom In button",
            });
          },

          iClickExecutionFlowZoomOutButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "icon",
                value: "sap-icon://zoom-out",
              }),
              actions: new Press(),
              errorMessage: "Cannot find the Zoom Out button",
            });
          },

          iClickExecutionFlowNode: function () {
            return this.waitFor({
              controlType: "sap.suite.ui.commons.ProcessFlowNode",
              matchers: new PropertyStrictEquals({
                name: "nodeId",
                value: "8781028f-e85a-584f-9e95-b878cbf6e921",
              }),
              actions: new Press(),
              errorMessage: "Cannot find the Execution Flow node",
            });
          },
        },

        assertions: {
          theDeliveryItemHasProcessStatusTag: function () {
            return this.waitFor({
              id: sProcessStatusTagId,
              controlType: "sap.m.GenericTag",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "As Planned",
              }),
              success: function () {
                Opa5.assert.ok(true, "The page has a generic tag");
              },
              errorMessage: "Does not have a generic tag",
            });
          },

          iShouldSeeTheHistoricalEventsPopover: function () {
            return this.waitFor({
              id: sHistoricalEventsPopoverId,
              controlType: "sap.m.Popover",
              success: function () {
                Opa5.assert.ok(true, "The popover is open");
              },
              errorMessage: "Cannot find the popover",
            });
          },

          theHistoricalEventsListHasItems: function () {
            return this.waitFor({
              id: sHistoricalEventsListId,
              matchers: new AggregationFilled({
                name: "items",
              }),
              success: function () {
                Opa5.assert.ok(true, "The historical events list has items");
              },
              errorMessage: "The historical events list does not have items",
            });
          },

          iShouldSeeTheMap: function () {
            return this.waitFor({
              id: sGeoMapId,
              controlType: "sap.ui.vbm.GeoMap",
              success: function () {
                Opa5.assert.ok(true, "The map is displayed");
              },
              errorMessage: "Cannot find the map",
            });
          },
        },
      },
    });
  }
);
