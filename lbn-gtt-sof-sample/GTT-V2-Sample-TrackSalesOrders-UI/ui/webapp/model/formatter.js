sap.ui.define(
  [
    "sap/ui/core/ValueState",
    "sap/ui/core/format/NumberFormat",
    "sap/ui/core/format/DateFormat",
    "sap/ui/vbm/SemanticType",
    "../constant/ProcessStatus",
    "../constant/ExecutionStatus",
    "../constant/NetworkGraph",
    "../constant/Event",
    "./type/ISODateTime",
  ],
  function (
    ValueState,
    NumberFormat,
    DateFormat,
    SemanticType,
    ProcessStatus,
    ExecutionStatus,
    NetworkGraph,
    Event,
    ISODateTime
  ) {
    "use strict";

    var formatter = {
      processStatus: ProcessStatus,
      executionStatus: ExecutionStatus,
      eventStatus: Event.Status,
      eventType: Event.Type,
      isoDateTimeFormatter: new ISODateTime(),

      floatNumberFormat: NumberFormat.getFloatInstance({
        maxFractionDigits: 2,
        minFractionDigits: 2,
        groupingEnabled: true,
      }, sap.ui.getCore().getConfiguration().getLocale()),

      floatNumberFormat3Decimals: NumberFormat.getFloatInstance({
        maxFractionDigits: 3,
        minFractionDigits: 3,
        groupingEnabled: true,
      }, sap.ui.getCore().getConfiguration().getLocale()),

      codeListDescription: function (localizedName, name, code) {
        return localizedName || name || code || "";
      },

      getCodeListDescriptionFromI18n: function (code, key) {
        var text = this.getText(key, null, "@i18n");

        return text === key ? code : text;
      },

      processStatusText: function (status) {
        if (!status) {
          return "";
        }

        var key = "CO_ProcessStatus_".concat(status).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, status, key);
      },

      executionStatusText: function (status) {
        if (!status) {
          return "";
        }

        var key = "CO_ExecutionStatus_".concat(status).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, status, key);
      },

      carrierRefDocumentTypeText: function (type) {
        if (!type) {
          return "";
        }

        if (type === "VP") {
          return this.getText("visibilityProvider");
        }

        var key = "CO_CarrierRefDocumentType_".concat(type).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, type, key);
      },

      eventStatusText: function (status) {
        if (!status) {
          return "";
        }

        var key = "CO_EventStatus_".concat(status).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, status, key);
      },

      vpLocationTypeText: function (type) {
        if (!type) {
          return "";
        }

        var key = "CO_VPLocationType_".concat(type).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, type, key);
      },

      eventTypeText: function (type) {
        if (!type) {
          return "";
        }

        var key = "ET_" + type + "_DESCR";
        var text = this.getText(key, null, "@i18n");

        return text === key ? type : text;
      },

      /**
       * Get last activity string
       *
       * @param {string} lastEventName Last event name
       * @param {string} [lastLocationDescription] Lats location description
       * @param {string} [lastVPLocationType] Last VP location type
       * @returns {string} Last activity
       */
      lastActivity: function (lastEventName, lastLocationDescription, lastVPLocationType) {
        var lastEventTypeText = formatter.eventTypeText.call(this, lastEventName);
        if (lastLocationDescription !== null) {
          return lastEventTypeText.concat(": ").concat(lastLocationDescription);
        } else if (lastVPLocationType !== null) {
          return lastEventTypeText.concat(": ").concat(lastVPLocationType);
        }
        return lastEventTypeText;
      },

      /**
       * Get last activity string
       *
       * @param {string} [statusLocalizedName] The execution status localized name
       * @param {string} [statusName] The execution status name
       * @param {string} [statusCode] The execution status code
       * @param {string} [lastEventName] Last event name
       * @param {string} [lastLocationDescription] Lats location description
       * @param {string} [lastVPLocationType] Last VP location type
       * @returns {string} Execution status with last activity
       */
      executionStatusWithLastActivity: function (statusLocalizedName, statusName, statusCode, lastEventName, lastLocationDescription, lastVPLocationType) {
        var description = "";
        var executionStatusText = formatter.codeListDescription(statusLocalizedName, statusName, statusCode);
        var lastActivityText = formatter.lastActivity.call(this, lastEventName, lastLocationDescription, lastVPLocationType);
        if (executionStatusText) {
          description = executionStatusText;
        }
        if (lastActivityText) {
          description += " (".concat(lastActivityText).concat(")");
        }

        return description;
      },

      delayStatus: function (value) {
        if (Number(value) === 0) {
          return ValueState.None;
        }

        return ValueState.Error;
      },

      fulfillmentStatusTooltip: function (item) {
        if (!item) {
          return "";
        }

        var processStatusText = formatter.processStatusText.call(this, item.status);
        return processStatusText.concat(" ").concat(item.value);
      },

      deliveryItemFulfillmentStatusTooltip: function (data) {
        var title = this.getText("deliveryItemFulfillmentStatusTooltip");
        if (!data) {
          return title;
        }

        return data.reduce(function (previous, current) {
          return previous.concat(
            formatter.eventStatusText.call(this, current.eventStatus_code)
              .concat(" ").concat(current.count).concat("\n")
          );
        }.bind(this), title.concat("\n"));
      },

      completionRateVisibility: function (completionValue, netValue) {
        if (completionValue === null || Number(netValue) === 0) {
          return false;
        }

        return true;
      },

      listCompletionRateVisibility: function (completionValue, netValue) {
        if (Number(completionValue) === 0 || Number(netValue) === 0) {
          return false;
        }

        return true;
      },

      formattedAddress: function (address) {
        if (address) {
          return address.replace(/\$/g, "\n");
        }

        return String.fromCharCode(parseInt(2013, 16));
      },

      formattedRate: function (value, total) {
        return formatter.getFormattedFloatNumber(formatter.getPercentValue(value, total));
      },

      formattedRateWithQuantityAndUnit: function (value, total, uom) {
        if (Number(value) === 0 || Number(total) === 0) {
          return "";
        }

        var rate = formatter.getFormattedFloatNumber(formatter.getPercentValue(value, total));
        return rate + "% (" + value + " " + uom + ")";
      },

      completionFraction: function (value, total) {
        return Number(((Number(value) / Number(total)) * 100).toFixed(2)); // 10.01 or 10
      },

      milestoneIcon: function (name) {
        switch (name) {
          case "salesOrderItemCreated":
            return "sap-icon://sales-order-item";
          case "salesOrderItemRejected":
            return "sap-icon://decline";
          case "salesOrderItemConfirmed":
            return "sap-icon://approvals";
          case "deliveryCreated":
            return "sap-icon://BusinessSuiteInAppSymbols/icon-outbound-delivery";
          case "deliveryGoodsIssued":
            return "sap-icon://activity-2";
          case "deliveryCompleted":
            return "sap-icon://complete";
        }

        return "sap-icon://process";
      },

      milestoneText: function (milestone) {
        if (!milestone) {
          return "";
        }

        return this.getText(milestone.name, [milestone.count, milestone.total]);
      },

      milestoneState: function (milestone) {
        var state = [];
        if (milestone) {
          var count = milestone.count;
          var total = milestone.total;
          var rejectCount = milestone.rejectCount;
          if (rejectCount > 0) {
            // sales order item confirmed with reject items
            state.push({
              state: "Negative",
              value: rejectCount,
            });
            state.push({
              state: "Positive",
              value: total - rejectCount,
            });
          } else if (milestone.name === "salesOrderItemRejected") {
            state.push({
              state: "Negative",
              value: count,
            });
          } else {
            state.push({
              state: "Positive",
              value: count,
            });
            state.push({
              state: "Neutral",
              value: total - count,
            });
          }
        }

        return state;
      },

      milestoneSummary: function (node) {
        if (!node) {
          return "";
        }

        return "<strong>100</strong> / 200 items";
      },

      getPercentValue: function (value, total) {
        if (Number(value) && Number(total)) {
          return Number(value / total * 100);
        }

        return 0;
      },

      getFormattedFloatNumber: function (value) {
        if (value !== null) {
          return formatter.floatNumberFormat.format(value);
        }

        return "";
      },

      /**
       * Get the time duration from start to end
       *
       * @param {Date} start Start date
       * @param {Date} end End date
       * @param {"complete"|"short"} [mode=complete] Time duration format mode
       * @returns {string} The time duration
       */
      timeDuration: function (start, end, mode) {
        var duration, seconds, minutes, hours, days, unit;
        var interval = Math.abs(end - start);

        seconds = Math.floor(interval / 1000);
        minutes = Math.floor(seconds / 60);
        hours = Math.floor(minutes / 60);
        days = Math.floor(hours / 24);

        switch (mode) {
          case "short":
            unit = {
              seconds: "secondsWithUnit",
              minutes: "minutesWithUnit",
              hours: "hoursWithUnit",
              days: "daysWithUnit",
            };
            break;
          case "complete":
          default:
            unit = {
              seconds: "seconds",
              minutes: "minutes",
              hours: "hours",
              days: "days",
            };
        }

        if (seconds < 60) {
          duration = this.getText(unit.seconds, [seconds]);
        } else if (minutes < 60) {
          duration = this.getText(unit.minutes, [minutes]);
          if (seconds % 60 !== 0) {
            duration = duration.concat(" ", this.getText(unit.seconds, [seconds % 60]));
          }
        } else if (minutes < 1440) {
          duration = this.getText(unit.hours, [hours]);
          if (minutes % 60 !== 0) {
            duration = duration.concat(" ", this.getText(unit.minutes, [minutes % 60]));
          }
        } else {
          duration = this.getText(unit.days, [days]);
          if (hours % 24 !== 0) {
            duration = duration.concat(" ", this.getText(unit.hours, [hours % 24]));
          }
        }
        return duration;
      },

      /**
       * Get the destination duration time tooltip string
       *
       * @param {Date} earliestETA Earliest ETA
       * @param {Date} plannedArrivalTime Planned arrival time
       * @returns {string} The destination duration
       */
      destinationDurationTooltip: function (earliestETA, plannedArrivalTime) {
        if (!earliestETA || !plannedArrivalTime) {
          return "";
        }

        return this.getText(
          (plannedArrivalTime < earliestETA) ? "lateEstimatedArrival" : "earlyEstimatedArrival",
          [formatter.timeDuration.call(this, earliestETA, plannedArrivalTime)]
        );
      },

      /**
       * Get the delta arrival time tooltip string
       *
       * @param {string} plannedArrivalTime Planned arrival time
       * @param {string} actualArrivalTime Actual arrival time
       * @returns {string} The delta arrival time tooltip string
       */
      deltaTimeTooltip: function (plannedArrivalTime, actualArrivalTime) {
        var dateTimeInstance = DateFormat.getDateTimeInstance();
        var planned = dateTimeInstance.parse(plannedArrivalTime);
        var actual = dateTimeInstance.parse(actualArrivalTime);

        return this.getText("plannedArrival").concat(": ").concat(plannedArrivalTime).concat("\n")
          .concat(this.getText("actualArrival")).concat(": ").concat(actualArrivalTime).concat("\n")
          .concat(this.getText("delta")).concat(": ").concat(
            this.getText(
              (actual >= planned) ? "later" : "earlier",
              [formatter.timeDuration.call(this, planned, actual)]
            )
          );
      },

      /**
       * Get the event type and location description
       * @example Arrival or Arrival: Shanghai or Arrival: 0001
       * @param {object} item The event data
       * @param {string} item.eventType EventType like Picking, Packing, Arrival
       * @param {string} item.locationTypeCode The vp location type code
       * @returns {string} The formatted description
       */
      eventTypeLocationDescription: function (item) {
        var description = "";
        if (item.eventType) {
          var eventTypeText = formatter.eventTypeText.call(this, item.eventType);
          description = eventTypeText;
        }

        var locationDescription = item.locationDescription;
        if (locationDescription) {
          return description.concat(": ").concat(locationDescription);
        }

        var locationTypeCode = item.locationTypeCode;
        if (locationTypeCode) {
          var locationTypeText = formatter.vpLocationTypeText.call(this, locationTypeCode);
          return description.concat(": ").concat(locationTypeText);
        }

        return description;
      },

      stopIcon: function (locationType) {
        switch(locationType) {
          case "ShippingPoint":
            return "sap-icon://journey-depart";
          case "Customer":
            return "sap-icon://visits";
          case "Supplier":
            return "sap-icon://factory";
          case "Plant":
            return "sap-icon://BusinessSuiteInAppSymbols/icon-warehouse";
          case "LogisticLocation":
            return "sap-icon://functional-location";
          default:
            return "";
        }
      },

      stopWithETATooltip: function (item) {
        var text = this.getText("stop", [item.locationDescription]);
        text += "\n" + this.getText("estimatedArrivalTime").concat(": ").concat(formatter.isoDateTimeFormatter.formatValue(item.estimatedArrival.estimatedArrivalTime, "string"));

        return text;
      },

      timelineItemStatus: function (type) {
        var eventStatusType = formatter.eventStatus.Type;
        switch (type) {
          case eventStatusType.DELAYED:
            return "Error";
          case eventStatusType.OVERDUE:
            return "Warning";
          case eventStatusType.PLANNED:
            return "Information";
          default:
            return "Success";
        }
      },

      networkGraphGroupNodeIcon: function (group) {
        switch (group) {
          case NetworkGraph.Group.SALES_ORDER:
            return "sap-icon://my-sales-order";
          case NetworkGraph.Group.SALES_ORDER_ITEM:
            return "sap-icon://sales-order-item";
          case NetworkGraph.Group.DELIVERY_ITEM:
            return "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck";
          case NetworkGraph.Group.DELIVERY:
            return "sap-icon://BusinessSuiteInAppSymbols/icon-outbound-delivery";
          case NetworkGraph.Group.SHIPMENT:
            return "sap-icon://BusinessSuiteInAppSymbols/icon-container-loading";
          case NetworkGraph.Group.RESOURCE:
            return "sap-icon://BusinessSuiteInAppSymbols/icon-container";
          default:
            return "";
        }
      },

      networkGraphNodeTitle: function (group, id) {
        var title = "";
        switch (group) {
          case NetworkGraph.Group.SALES_ORDER:
            title += this.getText("salesOrder") + ": ";
            break;
          case NetworkGraph.Group.SALES_ORDER_ITEM:
            title += this.getText("salesOrderItem") + ": ";
            break;
          case NetworkGraph.Group.DELIVERY_ITEM:
            title += this.getText("deliveryItem") + ": ";
            break;
          case NetworkGraph.Group.DELIVERY:
            title += this.getText("delivery") + ": ";
            break;
          case NetworkGraph.Group.SHIPMENT:
            title += this.getText("shipment") + ": ";
            break;
          case NetworkGraph.Group.RESOURCE:
            title += this.getText("resource") + ": ";
            break;
        }
        return title + id;
      },

      networkGraphNodeAttributeLabel: function (attribute) {
        if (!attribute) {
          return "";
        }

        var propertyName = attribute.propertyName;
        if (propertyName === "processStatus") {
          propertyName = "processStatus_code";
        } else if (propertyName === "executionStatus") {
          propertyName = "executionStatus_code";
        }

        var group = attribute.group;
        var entitySet = "";
        switch (group) {
          case NetworkGraph.Group.SALES_ORDER:
            entitySet = "SalesOrder";
            break;
          case NetworkGraph.Group.SALES_ORDER_ITEM:
            entitySet = "SalesOrderItem";
            break;
          case NetworkGraph.Group.DELIVERY_ITEM:
            entitySet = "DeliveryItem";
            break;
          case NetworkGraph.Group.DELIVERY:
            entitySet = "Delivery";
            break;
          case NetworkGraph.Group.SHIPMENT:
            entitySet = "Shipment";
            break;
          case NetworkGraph.Group.RESOURCE:
            entitySet = "Resource";
            break;
        }

        return this.getPropertyLabelText(propertyName, entitySet);
      },

      networkGraphNodeAttributeValue: function (attribute) {
        if (!attribute || attribute.value === null) {
          return String.fromCharCode(parseInt(2013, 16));
        }

        var propertyName = attribute.propertyName;
        var value = attribute.value;
        if (propertyName === "processStatus") {
          return formatter.processStatusText.call(this, value);
        }
        if (propertyName === "executionStatus") {
          return formatter.executionStatusText.call(this, value);
        }
        if (propertyName === "isDelayed") {
          return value === "true" ? this.getText("yes") : this.getText("no");
        }

        var uom = attribute.uom;
        if (uom) {
          var floatNumberFormat = formatter.floatNumberFormat;
          if (this.properiesWith3Scales.indexOf(propertyName) > -1) {
            floatNumberFormat = formatter.floatNumberFormat3Decimals;
          }
          var numberValue = floatNumberFormat.format(value);
          value = numberValue.concat(" ").concat(attribute.uom);
        }

        return value;
      },

      networkGraphGroupStatus: function (status) {
        return formatter.getElementStatus(status, "Group");
      },

      networkGraphNodeStatus: function (status) {
        return formatter.getElementStatus(status, "Node");
      },

      networkGraphLineStatus: function (status) {
        return formatter.getElementStatus(status, "Line");
      },

      getElementStatus: function (status, type) {
        if (status === "Warning") {
          return type.concat("StatusWarning");
        }
        return status;
      },

      spotType: function (eventStatusCode) {
        switch (eventStatusCode) {
          case formatter.eventStatus.Type.DELAYED:
            return SemanticType.Error;
          case formatter.eventStatus.Type.OVERDUE:
            return SemanticType.Warning;
        }

        return SemanticType.Default;
      },

      spotLabelType: function (eventStatusCode) {
        switch (eventStatusCode) {
          case formatter.eventStatus.Type.DELAYED:
            return SemanticType.Error;
          case formatter.eventStatus.Type.OVERDUE:
            return SemanticType.Warning;
        }

        return SemanticType.None;
      },

      spotLabelBorderColor: function (eventStatusCode) {
        switch (eventStatusCode) {
          case formatter.eventStatus.Type.DELAYED:
          case formatter.eventStatus.Type.OVERDUE:
            return "rgb(255, 255, 255)";
        }

        return "rgb(9, 97, 185)"; // informative
      },

    };

    return formatter;
  }
);
