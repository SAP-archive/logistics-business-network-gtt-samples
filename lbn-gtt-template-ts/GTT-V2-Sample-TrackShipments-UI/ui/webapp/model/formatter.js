sap.ui.define(
  [
    "sap/ui/core/format/NumberFormat",
    "sap/ui/vbm/library",
    "../util/i18n",
    "../util/dateDiff",
    "../constant/ProcessStatus",
    "../constant/ExecutionStatus",
    "../constant/Event",
    "../constant/TransportationMode",
    "./type/ISODateTime",
  ],
  function (
    NumberFormat,
    VBMLibrary,
    i18n,
    dateDiff,
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
        var text = i18n(key);

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
       * Get delta between two dates
       *
       * @param {string|number|Date} forecast Forecast date
       * @param {string|number|Date} actual Actual date
       * @returns {string} Date delta
       *
       * @example "+3D6H" | "-1H23M"
       */
      dateDelta: function (forecast, actual) {
        var forecastDate = new Date(forecast);
        var actualDate = new Date(actual);
        var sign = (actualDate >= forecastDate) ? "+" : "-";
        var diff = dateDiff(forecastDate, actualDate);

        if (!diff) {
          return "";
        }

        var detla = "";
        if (diff.seconds < 60) {
          detla = this.getText("unit.s", [diff.seconds]);
        } else if (diff.minutes < 60) {
          detla = this.getText("unit.m", [diff.minutes]);
          if (diff.seconds % 60 !== 0) {
            detla += this.getText("unit.s", [diff.seconds % 60]);
          }
        } else if (diff.hours < 24) {
          detla = this.getText("unit.h", [diff.hours]);
          if (diff.minutes % 60 !== 0) {
            detla += this.getText("unit.m", [diff.minutes % 60]);
          }
        } else {
          detla = this.getText("unit.d", [diff.days]);
          if (diff.hours % 24 !== 0) {
            detla += this.getText("unit.h", [diff.hours % 24]);
          }
        }

        return sign + detla;
      },

      /**
       * Get the ETA delta tooltip string
       *
       * @param {string|number|Date} eta ETA
       * @param {string|number|Date} planned Planned arrival time
       * @returns {string} ETA delta tooltip string
       *
       * @example "6 days 23 hours later than planned arrival time"
       */
      etaDeltaTooltip: function (eta, planned) {
        var etaDate = new Date(eta);
        var plannedDate = new Date(planned);
        var i18nPattern = (plannedDate < etaDate) ? "lateEstimatedArrival" : "earlyEstimatedArrival";
        var diff = dateDiff(plannedDate, etaDate);

        if (!diff) {
          return "";
        }

        var delta = "";
        if (diff.seconds < 60) {
          delta = this.getText("unit.seconds", [diff.seconds]);
        } else if (diff.minutes < 60) {
          delta = this.getText("unit.minutes", [diff.minutes]);
          if (diff.seconds % 60 !== 0) {
            delta = delta + " " + this.getText("unit.seconds", [diff.seconds % 60]);
          }
        } else if (diff.hours < 24) {
          delta = this.getText("unit.hours", [diff.hours]);
          if (diff.minutes % 60 !== 0) {
            delta = delta + " " + this.getText("unit.minutes", [diff.minutes % 60]);
          }
        } else {
          delta = this.getText("unit.days", [diff.days]);
          if (diff.hours % 24 !== 0) {
            delta = delta + " " + this.getText("unit.hours", [diff.hours % 24]);
          }
        }

        return this.getText(i18nPattern, [delta]);
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

      stopWithETALabelType: function (eventStatusCode) {
        switch (eventStatusCode) {
          case formatter.eventStatus.Type.DELAYED:
            return VBMLibrary.SemanticType.Error;
          case formatter.eventStatus.Type.OVERDUE:
            return VBMLibrary.SemanticType.Warning;
        }

        return VBMLibrary.SemanticType.None;
      },

      stopWithETALabelBorderColor: function (eventStatusCode) {
        switch (eventStatusCode) {
          case formatter.eventStatus.Type.DELAYED:
          case formatter.eventStatus.Type.OVERDUE:
            return "rgb(255, 255, 255)";
        }

        return "rgb(9, 97, 185)"; // informative
      },

      stopWithETATooltip: function (item) {
        var text = this.getText("stop", [item.location.locationDescription]);
        text += "\n" + this.getText("eta").concat(": ").concat(formatter.isoDateTimeFormatter.formatValue(item.estimatedArrival.estimatedArrivalTime, "string"));

        return text;
      },

      geoCoordinatesTooltip: function (lon, lat) {
        if (lon !== null && lat !== null) {
          return this.getText("geoCoordinatesTooltip", [lon, lat]);
        }
        return this.getText("geoCoordinatesMissing");
      },
    };

    return formatter;
  }
);
