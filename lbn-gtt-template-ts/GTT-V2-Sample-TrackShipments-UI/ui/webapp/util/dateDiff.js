sap.ui.define([
  "sap/base/assert",
], function (
  assert
) {
  "use strict";

  /**
   * Calculate the difference between two dates
   *
   * @param {Date} date1 Date1
   * @param {Date} date2 Date2
   * @returns {{
   *   weeks: number;
   *   days: number;
   *   hours: number;
   *   minutes: number;
   *   seconds: number;
   *   milliseconds: number;
   * }|undefined} Date difference
   */
  var dateDiff = function (date1, date2) {
    assert(date1 instanceof Date, "'" + date1 + "' is not a valid date.");
    assert(date2 instanceof Date, "'" + date2 + "' is not a valid date.");

    var milliseconds = Math.abs(date2 - date1);

    if (isNaN(milliseconds)) {
      return undefined;
    }

    var seconds = Math.floor(milliseconds / 1000);
    var minutes = Math.floor(seconds / 60);
    var hours = Math.floor(minutes / 60);
    var days = Math.floor(hours / 24);
    var weeks = Math.floor(days / 7);

    // TODO: Add months and years

    return {
      weeks: weeks,
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: seconds,
      milliseconds: milliseconds,
    };
  };

  return dateDiff;
});
