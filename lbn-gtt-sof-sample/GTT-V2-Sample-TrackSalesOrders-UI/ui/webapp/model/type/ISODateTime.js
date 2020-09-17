sap.ui.define(["sap/ui/model/type/DateTime"], function (DateTime) {
  "use strict";

  /**
   * Constructor for a ISO8601 DateTime type.
   *
   * @class
   * This class represents ISO8601 date time types.
   *
   * @extends sap.ui.model.type.DateTime
   *
   * @public
   * @param {object} [oFormatOptions] Formatting options. For a list of all available options, see {@link sap.ui.core.format.DateFormat.getDateTimeInstance DateFormat}.
   * @param {object} [oFormatOptions.source] Additional set of options used to create a second <code>DateFormat</code> object for conversions between
   *           string values in the data source (e.g. model) and <code>Date</code>. This second format object is used to convert from a model <code>string</code> to <code>Date</code> before
   *           converting the <code>Date</code> to <code>string</code> with the primary format object. Vice versa, this 'source' format is also used to format an already parsed
   *           external value (e.g. user input) into the string format that is expected by the data source.
   *           For a list of all available options, see {@link sap.ui.core.format.DateFormat.getDateTimeInstance DateFormat}.
   * @param {object} [oConstraints] Value constraints. Supports the same kind of constraints as its base type Date, but note the different format options (Date vs. DateTime).
   */
  return DateTime.extend("com.sap.gtt.app.sample.sof.model.type.ISODateTime", {
    constructor: function (oFormatOptions, oConstraints) {
      var formatOptions = oFormatOptions || {};
      formatOptions.source = {
        pattern: "yyyy-MM-dd'T'HH:mm:ssX",
      };

      DateTime.call(this, formatOptions, oConstraints);
      this.sName = "ISODateTime";
    },
  });
});
