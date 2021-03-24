sap.ui.define([
  "sap/ui/core/ValueState",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "sap/m/ValueColor",
  "sap/ui/core/format/DateFormat",
  "sap/ui/core/format/NumberFormat",
], function (ValueState, Constants, ValueColor, DateFormat, NumberFormat) {
  "use strict";

  var formatter = {

    floatNumberFormat: NumberFormat.getFloatInstance({
      decimals: 2,
      groupingEnabled: true,
    }, sap.ui.getCore().getConfiguration().getLocale()),

    floatNumberFormat3Decimals: NumberFormat.getFloatInstance({
      decimals: 3,
      groupingEnabled: true,
    }, sap.ui.getCore().getConfiguration().getLocale()),

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
          duration = duration + " " + this.getText(unit.seconds, [seconds % 60]);
        }
      } else if (minutes < 1440) {
        duration = this.getText(unit.hours, [hours]);
        if (minutes % 60 !== 0) {
          duration = duration + " " + this.getText(unit.minutes, [minutes % 60]);
        }
      } else {
        duration = this.getText(unit.days, [days]);
        if (hours % 24 !== 0) {
          duration = duration + " " + this.getText(unit.hours, [hours % 24]);
        }
      }
      return duration;
    },

    /**
     * Format tooltip for planned / actual arrival time.
     * @param {string} sPlannedArrivalTime planned arrival time
     * @param {string} sActualArrivalTime actual arrival time
     * @returns {string} delta arrival time tooltip
     */
    deltaTimeTooltip: function (sPlannedArrivalTime, sActualArrivalTime) {
      var oDateTimeInstance = DateFormat.getDateTimeInstance();
      var dPlanned = oDateTimeInstance.parse(sPlannedArrivalTime);
      var dActual = oDateTimeInstance.parse(sActualArrivalTime);

      return this.getText("plannedArrivalAtTooltip") + this.getText("labelDescriptionSeparator") + " " + sPlannedArrivalTime + "\n" +
        this.getText("actualArrivalAtTooltip") + this.getText("labelDescriptionSeparator") + " " + sActualArrivalTime + "\n" +
        this.getText("delta") + this.getText("labelDescriptionSeparator") + " " + this.getText(
        (dActual >= dPlanned) ? "later" : "earlier",
        [formatter.timeDuration.call(this, dPlanned, dActual)]
      );
    },

    getCodeListDescriptionFromI18n: function (sCode, sKey) {
      var sText = this.getText(sKey, null, "@i18n");

      return sText === sKey ? sCode : sText;
    },

    eventTypeText: function (sType) {
      if (!sType) {
        return "";
      }

      var sKey = "ET_" + sType + "_DESCR";
      var sText = this.getText(sKey, null, "@i18n");

      return sText === sKey ? sType : sText;
    },

    /**
     * Format date in range.
     * Start date will be formatted as the same day but with '00:00:00' (HH:mm:ss).
     * End date will be formatted as the next day with '00:00:00' (HH:mm:ss).
     * @param {object} oDateSettings
     *  - {object} date: date that should be formatted
     *  - {string} limit: 'start' | 'end'
     * @returns {string} formatted date
     */
    formatDateRange: function (oDateSettings) {
      if (!oDateSettings) {
        return "";
      }
      var dDate = oDateSettings.date;
      if(oDateSettings.limit === "end") {
        dDate = new Date(dDate.getFullYear(), dDate.getMonth(), dDate.getDate() + 1);
      }
      var oDateFormat = DateFormat.getDateInstance({
        pattern: oDateSettings.pattern || "yyyy-MM-dd",
      });
      return oDateFormat.format(dDate);
    },

    /**
     * Format rate string: add "%" if the value exists.
     * @param {string} sRate rate value
     * @returns {string} formatted rate
     */
    formatRate: function (sRate) {
      return sRate ? sRate + " %" : "";
    },

    /**
     * Format amount value with unit if it exists.
     * @param {string} sValue amount value
     * @param {string} sUnit unit string
     * @returns {string} amount with unit string
     */
    formatAmountUnitField: function (sValue, sUnit) {
      return sValue ? (sUnit ? sValue + " " + sUnit : sValue) : "";
    },

    /**
     * Format boolean field to -/Yes/No
     * @param {Boolean} bValue Boolean value of field
     * @returns {string} Text -/Yes/No for values true/false
     */
    formatBooleanFields: function (bValue) {
      if(!bValue && typeof bValue !== "boolean") {
        return "-";
      }
      return this.formatter.formatYesNo.call(this, bValue);
    },

    /**
     * Format boolean field to Yes/No
     * @param {Boolean} bValue Boolean value of field
     * @returns {string} Text Yes/No for values true/false
     */
    formatYesNo: function (bValue) {
      return bValue ? this.getText("yes") : this.getText("no");
    },

    /**
     * Format field with line breaks $ signs
     * @param {string} sValue Value with $ signs
     * @returns {string} Text with line breaks \n instead of $
     */
    formatLineBreaks: function (sValue) {
      if(!sValue) {
        return "-";
      }
      return sValue.replace(/\$/g, "\n");
    },

    /**
     * Format field with line breaks $ signs
     * @param {string} sParentNumber parent number
     * @param {string} sChildNumber child number
     * @returns {string} The template 'parentNumber / childNumber'
     */
    formatDetailTitle: function (sParentNumber, sChildNumber) {
      if(!sParentNumber && !sChildNumber) {
        return "-";
      }
      return (sParentNumber || "-") + " / " + (sChildNumber || "-");
    },

    formatEventStatusState: function (sEventStatusCode) {
      var sState;
      switch(sEventStatusCode) {
        case Constants.EVENT_STATUS_CODE.PLANNED:
          sState = ValueState.None;
          break;
        case Constants.EVENT_STATUS_CODE.DELAYED:
          sState = ValueState.Error;
          break;
        case Constants.EVENT_STATUS_CODE.OVERDUE:
        case Constants.EVENT_STATUS_CODE.LATE:
          sState = ValueState.Warning;
          break;
        case Constants.EVENT_STATUS_CODE.UNPLANNED:
        case Constants.EVENT_STATUS_CODE.REPORTED:
        case Constants.EVENT_STATUS_CODE.LATE_REPORTED:
        case Constants.EVENT_STATUS_CODE.EARLY_REPORTED:
          sState = ValueState.Success;
          break;
        default:
          sState = ValueState.None;
      }
      return sState;
    },

    formatEventStatusText: function (sEventStatusCode) {
      if (sEventStatusCode === Constants.EVENT_STATUS_CODE.UNPLANNED) {
        return this.getText("unplannedEventStatusLabel");
      }
      return this.formatter.getDescriptionText.call(this, sEventStatusCode, true);
    },

    formatEventTitle: function (oTimelineEvent) {
      var sTitleDescription = this.formatter.getDescriptionText.call(this, oTimelineEvent.eventType);
      if (oTimelineEvent.location && oTimelineEvent.location.locationDescription) {
        return sTitleDescription + this.getText("labelDescriptionSeparator") + " " + oTimelineEvent.location.locationDescription;
      } else if (oTimelineEvent.locationId) {
        return sTitleDescription + this.getText("labelDescriptionSeparator") + " " + oTimelineEvent.locationId;
      } else {
        return sTitleDescription;
      }
    },

    getDescriptionText: function (sKey, bIsEvent) {
      if(bIsEvent) {
        return this.getText("CO_EventStatus_" + sKey + "_NAME", null, "@i18n");
      } else {
        return this.getText("ET_" + sKey + "_DESCR", null, "@i18n");
      }
    },


    getProcessStatusText: function (sStatus) {
      if (!sStatus) {
        return "";
      }

      var sKey = "CO_ProcessStatus_" + sStatus + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);
    },

    getExecutionStatusText: function (sStatus) {
      if (!sStatus) {
        return "";
      }

      var sKey = "CO_ExecutionStatus_" + sStatus + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);
    },


    deliveryItemFulfillmentStatusTooltip: function (aData) {
      var sTitle = this.getText("deliveryItemFulfillmentStatusTooltip");

      if (!aData) {
        return sTitle;
      }

      return aData.reduce(function (sPrevious, oCurrent) {
        return sPrevious + formatter.eventStatusText.call(this, oCurrent.eventStatus_code) + " " + oCurrent.count + "\n";
      }.bind(this), sTitle + "\n");
    },

    eventStatusText: function (sStatus) {
      if (!sStatus) {
        return "";
      }

      var sKey = "CO_EventStatus_" + sStatus + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);
    },

    processStatusText: function (sStatus) {
      if (!sStatus) {
        return "";
      }

      var sKey = "CO_EventStatus_" + sStatus + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);
    },

    getProcessStatusState: function (sStatus) {
      switch(sStatus) {
        case Constants.PROCESS_STATUS_CODE.AS_PLANNED:
          return ValueState.Success;
        case Constants.PROCESS_STATUS_CODE.LATE:
        case Constants.PROCESS_STATUS_CODE.EARLY:
        case Constants.PROCESS_STATUS_CODE.OVERDUE:
          return ValueState.Warning;
        case Constants.PROCESS_STATUS_CODE.DELAYED:
          return ValueState.Error;
        default:
          return ValueState.None;
      }
    },

    getExecutionStatusState: function (sStatus) {
      switch(sStatus) {
        case Constants.EXECUTION_STATUS_CODE.NOT_STARTED:
          return ValueState.None;
        case Constants.EXECUTION_STATUS_CODE.IN_TRANSIT:
          return ValueState.Information;
        case Constants.EXECUTION_STATUS_CODE.COMPLETED:
          return ValueState.Success;
        default:
          return ValueState.None;
      }
    },

    getColor: function (sStatus) {
      switch (sStatus) {
        case Constants.EVENT_STATUS_CODE.PLANNED:
          return ValueColor.Neutral;
        case Constants.EVENT_STATUS_CODE.OVERDUE:
          return ValueColor.Critical;
        case Constants.EVENT_STATUS_CODE.REPORTED:
          return ValueColor.Good;
        case Constants.EVENT_STATUS_CODE.DELAYED:
          return ValueColor.Error;
        default:
          return ValueColor.Neutral;
      }
    },

    listCompletionRateVisibility: function (sCompletionValue, sNetValue) {
      return !(Number(sCompletionValue) === 0 || Number(sNetValue) === 0);
    },

    completionFraction: function (sValue, sTotal) {
      if(Number(sValue) === 0 || Number(sTotal) === 0) {
        return 0;
      }
      return Number(((Number(sValue) / Number(sTotal)) * 100).toFixed(2));
    },

    /**
     * Format vp location type string
     * @param {string} sType vp location type
     * @returns {string} formatted value from i18n
     */
    formatVpLocationTypeText: function (sType) {
      if (!sType) {
        return "";
      }

      var sKey = "CO_VPLocationType_" + sType + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sType, sKey);
    },

    lastReportedEventStatusText: function (sStatus) {
      if (!sStatus) {
        return "–";
      }

      var sKey = "ET_" + sStatus.split(".").pop() + "_DESCR";
      return formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);
    },

    /**
     * Format last reported event value
     * @param {string} sStatus execution status code value
     * @param {string} sLastLocationDescription last location description
     * @param {string} sLastVPLocationType last VP location type value
     * @returns {string} formatted last reported event value for table column
     */
    lastReportedEventStatusTableText: function (sStatus, sLastLocationDescription, sLastVPLocationType) {
      if (!sStatus) {
        return "";
      }

      var sKey = "ET_" + sStatus + "_DESCR";
      var sLastEventTypeText = formatter.getCodeListDescriptionFromI18n.call(this, sStatus, sKey);

      if (sLastLocationDescription) {
        return sLastEventTypeText + this.getText("labelDescriptionSeparator") + " " + sLastLocationDescription;
      } else if (sLastVPLocationType) {
        return sLastEventTypeText + this.getText("labelDescriptionSeparator") + " " + formatter.formatVpLocationTypeText.call(this, sLastVPLocationType);
      }
      return sLastEventTypeText;
    },

    /**
     * Format execution status column value
     * @param {string} sExecutionStatus execution status code value
     * @param {string} sLastReportedEvent last reported event name
     * @param {string} sLastLocationDescription last location description
     * @param {string} sLastVPLocationType last VP location type value
     * @returns {string} formatted execution status value for table column
     */
    getTableExecutionStatusText: function (sExecutionStatus, sLastReportedEvent, sLastLocationDescription, sLastVPLocationType) {
      if(!sLastReportedEvent) {
        return formatter.getExecutionStatusText.call(this, sExecutionStatus);
      }

      return formatter.getExecutionStatusText.call(this, sExecutionStatus) + " (" +
      formatter.lastReportedEventStatusTableText.call(this, sLastReportedEvent, sLastLocationDescription, sLastVPLocationType) + ")";
    },

    /**
     * Format execution status generic tag tooltip value in header
     * @param {string} sExecutionStatus execution status code value
     * @param {string} sLastReportedEvent last reported event name
     * @param {string} sLastLocationDescription last location description
     * @param {string} sLastVPLocationType last VP location type value
     * @returns {string} formatted execution status tooltip value for inbound delivery item header
     */
    getHeaderExecutionStatusTooltip: function (sExecutionStatus, sLastReportedEvent, sLastLocationDescription, sLastVPLocationType) {
      if(!sLastReportedEvent) {
        return formatter.getExecutionStatusText.call(this, sExecutionStatus);
      }

      return formatter.lastReportedEventStatusTableText.call(this, sLastReportedEvent, sLastLocationDescription, sLastVPLocationType);
    },

    /**
     * Format execution status generic tag text value in header
     * @param {string} sExecutionStatus execution status code value
     * @param {string} sLastReportedEvent last reported event name
     * @returns {string} formatted execution status value for inbound delivery item header
     */
    getHeaderExecutionStatusText: function (sExecutionStatus, sLastReportedEvent) {
      if(!sLastReportedEvent) {
        return formatter.getExecutionStatusText.call(this, sExecutionStatus);
      }

      var sKey = "ET_" + sLastReportedEvent + "_DESCR";
      var sLastEventTypeText = formatter.getCodeListDescriptionFromI18n.call(this, sLastReportedEvent, sKey);

      return sLastEventTypeText;
    },

    carrierRefDocumentTypeText: function (sType) {
      if (!sType) {
        return "";
      }

      if (sType === Constants.REF_BUSINESS_DOCUMENTS_TYPE.VP) {
        return this.getText("visibilityProvider");
      }

      var sKey = "CO_CarrierRefDocumentType_" + sType + "_NAME";
      return formatter.getCodeListDescriptionFromI18n.call(this, sType, sKey);
    },

    getExecutionStatusIcon: function (sStatus) {
      switch(sStatus) {
        case Constants.EXECUTION_STATUS_CODE.NOT_STARTED:
          return "sap-icon://status-inactive";
        case Constants.EXECUTION_STATUS_CODE.IN_TRANSIT:
          return "sap-icon://status-in-process";
        case Constants.EXECUTION_STATUS_CODE.COMPLETED:
          return "sap-icon://status-completed";
        default:
          return "sap-icon://status-inactive";
      }
    },

    /**
     * Format delayed value for PO table
     * @param {String} sValue delayed value
     * @param {String} sTotal total net value
     * @param {String} sUom unit of measure
     *
     * @returns {String} formatted delayed value
     */
    formattedDelayedValue: function (sValue, sTotal, sUom) {
      if (Number(sValue) === 0 || Number(sTotal) === 0) {
        return "";
      }

      var rate = formatter.getFormattedFloatNumber(formatter.getPercentValue(sValue, sTotal));
      return rate + "% (" + formatter.floatNumberFormat.format(sValue) + " " + sUom + ")";
    },

    /**
     * Format delayed quantity for PO items table
     * @param {String} sValue delayed quantity
     * @param {String} sTotal total order quantity
     * @param {String} sUom unit of measure
     *
     * @returns {String} formatted delayed quantity
     */
    formattedDelayedQuantity: function (sValue, sTotal, sUom) {
      if (Number(sValue) === 0 || Number(sTotal) === 0) {
        return "";
      }

      var rate = formatter.getFormattedFloatNumber(formatter.getPercentValue(sValue, sTotal));
      return rate + "% (" + formatter.floatNumberFormat3Decimals.format(sValue) + " " + sUom + ")";
    },

    /**
     * Format float number with 2 decimals
     * @param {String} sValue value of number
     *
     * @returns {String} formatted float number with 2 decimals
     */
    getFormattedFloatNumber: function (sValue) {
      if (sValue !== null) {
        return formatter.floatNumberFormat.format(sValue);
      }

      return "";
    },

    /**
     * Format float number with 3 decimals
     * @param {String} sValue value of number
     *
     * @returns {String} formatted float number with 3 decimals
     */
    getFormatted3DFloatNumber: function (sValue) {
      if (sValue !== null) {
        return formatter.floatNumberFormat3Decimals.format(sValue);
      }

      return "";
    },

    /**
     * Get percentage of value from total
     * @param {String} sValue value of number
     * @param {String} sTotal total value
     *
     * @returns {Number} number of percent value to total
     */
    getPercentValue: function (sValue, sTotal) {
      if (Number(sValue) && Number(sTotal)) {
        return Number(sValue / sTotal * 100);
      }

      return 0;
    },

    /**
     * Get color status according to value
     * @param {String} sValue value of number
     *
     * @returns {String} status color
     */
    delayStatus: function (sValue) {
      if (Number(sValue) === 0) {
        return ValueState.None;
      }

      return ValueState.Error;
    },

    /**
     * Get formatted float percentage of value from total
     * @param {String} sValue value of number
     * @param {String} sTotal total value
     *
     * @returns {String} formatted float percentage of value from total
     */
    formattedRate: function (sValue, sTotal) {
      return formatter.getFormattedFloatNumber(formatter.getPercentValue(sValue, sTotal));
    },

    /** Return Error value state for the row in case of negative value.
     * @param {boolean} bIsNegative is negative value
     * @returns {string} error - if there is a value, none - if there is no value
     */
    formatRowHighlight: function (bIsNegative) {
      return bIsNegative ? ValueState.Error : ValueState.None;
    },

    /** Return completedAndLate Value/Quantity if exists
     * @param {String} sValue value of completedAndLate Value/Quantity field
     * @returns {string} Value if there is a value, if not - "–"
     */
    formatCompletedAndLateValueAndQuantityColumn: function (sValue) {
      return sValue && sValue !== "0.00" && sValue !== "0.000" ? sValue : "–";
    },

    /** Return unit of measure of completedAndLate Value/Quantity if exists
     * @param {String} sValue value or completedAndLateQuantity field
     * @param {String} sUnit unit of measure
     * @returns {string} Unit of measure if there is a value, if not - ""
     */
    formatCompletedAndLateUnit: function (sValue, sUnit) {
      return sValue && sValue !== "0.00" && sValue !== "0.000" ? sUnit : "";
    },
  };
  return formatter;
});
