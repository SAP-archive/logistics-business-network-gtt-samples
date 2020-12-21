sap.ui.define(
  [
    "sap/ui/test/Opa5",
    "sap/ui/test/matchers/AggregationFilled",
    "sap/ui/test/actions/Press",
    "sap/ui/test/matchers/PropertyStrictEquals",
    "sap/ui/test/actions/EnterText",
  ],
  function (
    Opa5,
    AggregationFilled,
    Press,
    PropertyStrictEquals,
    EnterText
  ) {
    "use strict";

    var sShipmentHeadingTitleId = /(.*)shipment--expandedHeadingTitle$/;
    var sReportingHistoryPopoverId = /(.*)reportingHistoryPopover--reportingHistoryPopover-popover$/;
    var sHistoricalEventsListId = /(.*)reportingHistoryPopover--historicalEventsList$/;
    var sReportEventsActionSheetId = /(.*)trackingTimelineView--reportEventsActionSheet--actionSheet$/;
    var sReportEventsDialogId = /(.*)reportEventsDialog--dialog$/;
    var sPlannedEventInputBoxId = /(.*)reportEventsDialog--plannedEvent$/;
    var sTheFirstPlannedEventSelectionId = /(.*)reportEventsDialog--plannedEvent-0$/;
    var sUserDefinedFieldsFormId = /(.*)reportEventsDialog--userDefinedFieldsForm$/;
    var sReportedById = /(.*)reportEventsDialog--reportedBy$/;

    Opa5.createPageObjects({

      onTheShipmentPage: {
        actions: {
          iClickReportingHistoryLink: function () {
            return this.waitFor({
              controlType: "sap.m.Link",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Reporting History (1)",
              }),
              actions: new Press(),
              errorMessage: "Does not have an event with reporting history",
            });
          },

          iPressOnReportButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Report",
              }),
              actions: new Press(),
              errorMessage: "Does not have a Report Button",
            });
          },

          iPressOnReportPlannedEventsButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Report Planned Event",
              }),
              actions: new Press(),
              errorMessage: "Does not have a Report Planned Events Button",
            });
          },

          iClickPlannedEventInputBox: function () {
            return this.waitFor({
              id: sPlannedEventInputBoxId,
              controlType: "sap.m.Select",
              actions: new Press(),
              errorMessage: "Does not have a planned event input box",
            });
          },

          iSelectTheFirstPlannedEvent: function () {
            return this.waitFor({
              id: sTheFirstPlannedEventSelectionId,
              controlType: "sap.ui.core.Item",
              actions: new Press(),
              errorMessage: "Does not have such a planned event selection",
            });
          },

          iReportedBy: function () {
            return this.waitFor({
              id: sReportedById,
              controlType: "sap.m.Input",
              actions: new EnterText({
                text: "alex@qq.com",
              }),
              errorMessage: "Cannot find the Input field.",
            });
          },

          iPressOnCancelReportEventButton: function () {
            return this.waitFor({
              controlType: "sap.m.Button",
              matchers: new PropertyStrictEquals({
                name: "text",
                value: "Cancel",
              }),
              actions: new Press(),
              errorMessage: "Does not have a Cancel Report Button",
            });
          },
        },
        assertions: {
          iShouldSeeTheHeadingTitle: function () {
            return this.waitFor({
              id: sShipmentHeadingTitleId,
              controlType: "sap.m.Title",
              success: function () {
                Opa5.assert.ok(true, "The shipment heading title is found");
              },
              errorMessage: "Cannot find the shipment heading title",
            });
          },

          iShouldSeeTheReportingHistoryPopover: function () {
            return this.waitFor({
              id: sReportingHistoryPopoverId,
              controlType: "sap.m.Popover",
              success: function () {
                Opa5.assert.ok(true, "The reporting history popover is opened");
              },
              errorMessage: "Cannot find the reporting history popover",
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

          iShouldSeeTheReportEventsActionSheet: function () {
            return this.waitFor({
              id: sReportEventsActionSheetId,
              controlType: "sap.m.ActionSheet",
              success: function () {
                Opa5.assert.ok(true, "The report events action sheet is opened");
              },
              errorMessage: "Cannot find the report events action sheet",
            });
          },

          iShouldSeeTheReportEventsDialog: function () {
            return this.waitFor({
              id: sReportEventsDialogId,
              controlType: "sap.m.Dialog",
              success: function () {
                Opa5.assert.ok(true, "The report events dialog is opened");
              },
              errorMessage: "Cannot find the report events dialog",
            });
          },

          iShouldSeeTheUserDefinedFieldsForm: function () {
            return this.waitFor({
              id: sUserDefinedFieldsFormId,
              controlType: "sap.ui.layout.form.Form",
              success: function () {
                Opa5.assert.ok(true, "The user defined fields form is loaded");
              },
              errorMessage: "Cannot find the user defined fields form",
            });
          },
        },
      },

    });
  }
);
