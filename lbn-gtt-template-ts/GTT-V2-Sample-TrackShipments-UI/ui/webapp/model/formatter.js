sap.ui.define(
  [
    "sap/ui/core/format/NumberFormat",
    "../constant/ProcessStatus",
    "../constant/ExecutionStatus",
    "../constant/Event",
    "../constant/TransportationMode",
    "./type/ISODateTime",
  ],
  function (
    NumberFormat,
    ProcessStatus,
    ExecutionStatus,
    Event,
    TransportationMode,
    ISODateTime
  ) {
    "use strict";

    var formatter = {
      processStatus: ProcessStatus,
      executionStatus: ExecutionStatus,
      eventStatus: Event.Status,
      transportationMode: TransportationMode,
      isoDateTimeFormatter: new ISODateTime(),

      floatNumberFormat: NumberFormat.getFloatInstance({
        maxFractionDigits: 2,
        minFractionDigits: 2,
        groupingEnabled: true,
      }, sap.ui.getCore().getConfiguration().getLocale()),

      formatQuantityToTwoDecimalWithUom: function (value, uom, isOnList) {
        if (value === null || value === undefined || isNaN(value)) {
          return isOnList ? "" : String.fromCharCode(parseInt(2013, 16));
        }

        var measure = uom || "";
        return ((formatter.floatNumberFormat.format(parseFloat(value)) + " " + measure).trim());
      },

      /**
       * Format the gross and net duration values
       * @param {string} duration `HHHHHMMSS`
       * @param {string} isOnList Different display between list and information tab for a null value
       * @return {string} Formatted string like `HHHHH:MM:SS`
       */
      formatGrossNetDuration: function (duration, isOnList) {
        if (!duration) {
          return isOnList ? "" : String.fromCharCode(parseInt(2013, 16));
        }
        var seconds = duration.substr(-2, 2);
        var minutes = duration.substr(-4, 2);
        var hours = duration.substr(0, duration.length - 4);

        return [hours, minutes, seconds].join(":").replace(/^:/, "");
      },

      registrationCountryRegionAndNumber: function (country, number) {
        if (country && number) {
          return country.concat(" ").concat(number);
        }

        return country || number || String.fromCharCode(parseInt(2013, 16));
      },

      codeListDescription: function (localizedName, name, code) {
        return localizedName || name || code || "";
      },

      getCodeListDescriptionFromI18n: function (code, key) {
        var text = this.getText(key, null, "@i18n");

        return text === key ? code : text;
      },

      eventStatusText: function (status) {
        if (!status) {
          return "";
        }

        if (status === formatter.eventStatus.Type.UNPLANNED) {
          return this.getText("unplanned");
        }

        var key = "CO_EventStatus_".concat(status).concat("_NAME");
        return formatter.getCodeListDescriptionFromI18n.call(this, status, key);
      },

      eventTypeText: function (type) {
        if (!type) {
          return "";
        }

        var key = "ET_" + type + "_DESCR";
        var text = this.getText(key, null, "@i18n");

        return text === key ? type : text;
      },

      formattedAddress: function (address) {
        if (address) {
          return address.replace(/\$/g, "\n");
        }

        return String.fromCharCode(parseInt(2013, 16));
      },

      /**
       * Turn timestamp string into DateTime object
       *
       * @param {string} timestamp ISO timestamp
       * @returns {Date} Date formatted
       */
      dateTime: function (timestamp) {
        return formatter.isoDateTimeFormatter.formatValue(timestamp, "string");
      },

      /**
       * Get the time duration from start to end
       *
       * @param {string|number|Date} start Start date
       * @param {string|number|Date} end End date
       * @param {"complete"|"short"} [mode=complete] Time duration format mode
       * @returns {string} The time duration
       */
      timeDuration: function (start, end, mode) {
        var duration, seconds, minutes, hours, days, unit;
        var interval = Math.abs(new Date(end) - new Date(start));

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
       * Get the ETA delta tooltip string
       *
       * @param {string|number|Date} eta ETA
       * @param {string|number|Date} planned Planned arrival time
       * @returns {string} ETA delta tooltip string
       */
      etaDetlaTooltip: function (eta, planned) {
        var etaDate = new Date(eta);
        var plannedDate = new Date(planned);

        return this.getText(
          (plannedDate < etaDate) ? "lateEstimatedArrival" : "earlyEstimatedArrival",
          [formatter.timeDuration.call(this, etaDate, plannedDate)]
        );
      },

      shipmentEventsByStatusTooltip: function (data) {
        var title = this.getText("shipmentEventsByStatusTooltip");
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

      /**
       * Get summary of an event
       *
       * @param {string} eventType event type
       * @param {string} [locationDescription] Location description
       * @param {string} [locationType] Location type name
       * @returns {string} Event summary
       */
      eventSummary: function (eventType, locationDescription, locationType) {
        var eventTypeText = formatter.eventTypeText.call(this, eventType);

        if (locationDescription) {
          return eventTypeText.concat(": ").concat(locationDescription);
        } else if (locationType) {
          return eventTypeText.concat(": ").concat(locationType);
        }

        return eventTypeText;
      },

      stopPosition: function (location) {
        if (location) {
          return [location.longitude, location.latitude, 0].join(";");
        }

        return "";
      },

      stopIcon: function (locationId, isSource, departureLocationId, arrivalLocationId) {
        if (locationId === departureLocationId && isSource) {
          return "sap-icon://arrow-top";
        }

        if (locationId === arrivalLocationId) {
          return "sap-icon://arrow-bottom";
        }

        return "sap-icon://functional-location";
      },

      stopTooltip: function (locationId, isSource, departureLocationId, arrivalLocationId, locationDescription) {
        var label = "";
        if (locationId === departureLocationId && isSource) {
          label = this.getText("sourceLocation");
        } else if (locationId === arrivalLocationId) {
          label = this.getText("destinationLocation");
        } else {
          label = this.getText("intermediateLocation");
        }

        return label.concat(": ").concat(locationDescription);
      },

      stopWithETATooltip: function (item) {
        var text = this.getText("stop", [item.location.locationDescription]);
        text += "\n" + this.getText("eta").concat(": ").concat(formatter.isoDateTimeFormatter.formatValue(item.estimatedArrival.estimatedArrivalTime, "string"));

        return text;
      },
    };

    return formatter;
  }
);
