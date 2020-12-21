sap.ui.define([
  "com/sap/gtt/app/sample/sst/model/formatter",
  "sap/base/strings/formatMessage",
  "sap/m/ValueColor",
  "sap/ui/core/ValueState",
  "com/sap/gtt/app/sample/sst/model/type/ISODate",
  "com/sap/gtt/app/sample/sst/model/type/ISODateTime",
], function (formatter, formatMessage, ValueColor, ValueState, ISODate, ISODateTime) {
  "use strict";

  var sandbox = sinon.createSandbox();

  function stub(object, method, func) {
    if (!(method in object)) {
      object[method] = function () {};
    }

    var stubbed = sandbox.stub(object, method);

    if (typeof func === "function") {
      return stubbed.callsFake(func);
    }

    return stubbed;
  }

  QUnit.module("com.sap.gtt.app.sst.model.formatter", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("timeDuration", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");

    // Act
    var start = new Date("1995-12-17T03:24:00");
    formatter.timeDuration(start, new Date("1995-12-17T03:24:30")); // +30s
    formatter.timeDuration(start, new Date("1995-12-17T03:25:00")); // +1m
    formatter.timeDuration(start, new Date("1995-12-17T07:24:00")); // +4h
    formatter.timeDuration(start, new Date("1995-12-19T03:24:00")); // +2d

    // Assert
    assert.deepEqual(formatter.getText.args[0], ["seconds", [30]], "The 'seconds' text is got from i18n");
    assert.deepEqual(formatter.getText.args[1], ["minutes", [1]], "The 'minutes' text is got from i18n");
    assert.deepEqual(formatter.getText.args[2], ["hours", [4]], "The 'hours' text is got from i18n");
    assert.deepEqual(formatter.getText.args[3], ["days", [2]], "The 'days' text is got from i18n");
  });

  QUnit.test("ISODate, ISODateTime", function (assert) {
    var isoDate = new ISODate();
    var isoDateTime = new ISODateTime();

    var date = isoDate.formatValue("2011-10-02", "string");
    var datetime = isoDateTime.formatValue("2011-10-02T12:20:30.000Z", "string");

    assert.ok(date, "The formatted date is got");
    assert.ok(datetime, "The formatted datetime is got");
  });
});
