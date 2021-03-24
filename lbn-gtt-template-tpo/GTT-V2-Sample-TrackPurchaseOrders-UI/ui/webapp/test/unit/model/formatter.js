sap.ui.define([
  "com/sap/gtt/app/sample/pof/model/formatter",
  "sap/ui/core/format/DateFormat",
  "sap/m/ValueColor",
  "sap/ui/core/ValueState",
  "sap/base/strings/formatMessage",
], function (formatter, DateFormat, ValueColor, ValueState, formatMessage) {
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

  QUnit.module("com.sap.gtt.app.pof.model.formatter", {
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
    var aModes = ["short", "complete","wrongMode"];
    var start = new Date("1995-12-17T03:24:00");
    formatter.timeDuration(start, new Date("1995-12-17T03:24:30")); // +30s
    formatter.timeDuration(start, new Date("1995-12-17T03:25:00")); // +1m
    formatter.timeDuration(start, new Date("1995-12-17T07:24:00")); // +4h
    formatter.timeDuration(start, new Date("1995-12-19T03:24:00")); // +2d
    formatter.timeDuration(start, new Date("1995-12-19T03:24:00"), aModes[0]); // +2d
    formatter.timeDuration(start, new Date("1995-12-19T03:24:00"), aModes[1]); // +2d
    formatter.timeDuration(start, new Date("1995-12-19T03:24:00"), aModes[2]); // +2d

    // Assert
    assert.deepEqual(formatter.getText.args[0], ["seconds", [30]], "The 'seconds' text is got from i18n");
    assert.deepEqual(formatter.getText.args[1], ["minutes", [1]], "The 'minutes' text is got from i18n");
    assert.deepEqual(formatter.getText.args[2], ["hours", [4]], "The 'hours' text is got from i18n");
    assert.deepEqual(formatter.getText.args[3], ["days", [2]], "The 'days' text is got from i18n");
    assert.deepEqual(formatter.getText.args[4], ["daysWithUnit", [2]], "The 'daysWithUnit' text is got from i18n");
    assert.deepEqual(formatter.getText.args[5], ["days", [2]], "The 'days' text is got from i18n");
    assert.deepEqual(formatter.getText.args[6], ["days", [2]], "The 'days' text is got from i18n");

    formatter.getText.restore();
  });

  QUnit.test("formatEventStatusText", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");

    // Act
    formatter.formatEventStatusText("UNPLANNED");

    // Assert
    assert.deepEqual(formatter.getText.args[0], ["unplannedEventStatusLabel"], "The 'unplannedEventStatusLabel' text is got from i18n");


    formatter.getText.restore();
  });

  QUnit.test("eventStatusText", function (assert) {
    assert.equal(formatter.eventStatusText(null), "", "The '' text because of null entry ");
  });

  QUnit.test("processStatusText", function (assert) {
    assert.equal(formatter.processStatusText(null), "", "The '' text because of null entry ");
  });

  QUnit.test("getProcessStatusState", function (assert) {
    assert.equal(formatter.getProcessStatusState("DELAYED"), ValueState.Error, "The 'Error' text because of DELAYED entry ");
  });

  QUnit.test("eventTypeText", function (assert) {
    assert.equal(formatter.eventTypeText(null), "", "The '' text because of null entry ");
  });

  QUnit.test("formatDetailTitle", function (assert) {
    assert.equal(formatter.formatDetailTitle(null,null), "-", "The '-' text because of null entry ");
    assert.equal(formatter.formatDetailTitle("0001",null), "0001 / -", "The '0001 / -' text because of second parameter null ");
    assert.equal(formatter.formatDetailTitle(null,"10"), "- / 10", "The '- / 10' text because of first parameter null ");
    assert.equal(formatter.formatDetailTitle("0001","10"), "0001 / 10", "The '0001 / 10' text ");
  });

  QUnit.test("formatEventStatusState", function (assert) {
    assert.equal(formatter.formatEventStatusState(null), ValueState.None, "The None value because of null entry ");
    assert.equal(formatter.formatEventStatusState("UNPLANNED"), ValueState.Success, "The Success value because of UNPLANNED entry ");
    assert.equal(formatter.formatEventStatusState("REPORTED"), ValueState.Success, "The Success value because of REPORTED entry ");
  });

  QUnit.test("formatYesNo", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");

    // Act
    formatter.formatYesNo(true);
    formatter.formatYesNo(false);

    // Assert
    assert.deepEqual(formatter.getText.args[0], ["yes"], "The 'Yes' text is got from i18n");
    assert.deepEqual(formatter.getText.args[1], ["no"], "The 'No' text is got from i18n");


    formatter.getText.restore();
  });

  QUnit.test("formatDateRange", function (assert) {
    // Assert
    assert.equal(formatter.formatDateRange({date: new Date("2020-09-11"), limit: "end"}), "2020-09-12", "Passed {date, limit: 'end'}: Return one day after the passed date (without the time)");
    assert.equal(formatter.formatDateRange({date: new Date("2020-09-11"), limit: "start"}), "2020-09-11", "Passed {date, limit: 'start'}: Return the same date (without the time)");
    assert.equal(formatter.formatDateRange({date: new Date("2020-09-11")}), "2020-09-11", "Passed {date}: Return the same date (without the time)");
    assert.equal(formatter.formatDateRange({}), "", "Passed {}: Return the empty string");
    assert.equal(formatter.formatDateRange(), "", "No arguments: Return the empty string");
  });

  QUnit.test("formatRate", function (assert) {
    // Assert
    assert.equal(formatter.formatRate("10"), "10 %", "Rate with 10 formatted as - 10 %");
    assert.equal(formatter.formatRate(), "", "No Rate value return empty string");
    assert.notEqual(formatter.formatRate("10"), "10%", "Rate with 10 shouldn't be formatted as - 10%");
  });

  QUnit.test("formatAmountUnitField", function (assert) {
    // Assert
    assert.equal(formatter.formatAmountUnitField("100"), "100", "Return just amount, if unit doesn't exist");
    assert.equal(formatter.formatAmountUnitField("100", "KG"), "100 KG", "Return amount with unit, if they both exist");
    assert.equal(formatter.formatAmountUnitField(undefined, undefined), "", "No amount and no unit return empty string");
    assert.equal(formatter.formatAmountUnitField(undefined, "KG"), "", "No amount return empty string even unit exist");
    assert.notEqual(formatter.formatAmountUnitField("100", "KG"), "100KG", "Return amount with unit and space between them");
  });

  QUnit.test("formatLineBreaks", function (assert) {
    // Assert
    assert.equal(formatter.formatLineBreaks("City$Street$House$Flat"), "City\nStreet\nHouse\nFlat", "Return formatted string with \\n instead of $");
    assert.equal(formatter.formatLineBreaks("$ Street $ House$ Flat"), "\n Street \n House\n Flat", "Return formatted string with \\n instead of $");
    assert.equal(formatter.formatLineBreaks(undefined), "-", "Return '-' in case if parameter is undefined");
    assert.equal(formatter.formatLineBreaks(null), "-", "Return '-' in case if parameter is null");
    assert.equal(formatter.formatLineBreaks("City Street House Flat"), "City Street House Flat", "Return unformatted string");
    assert.notEqual(formatter.formatLineBreaks("City $ Street $ House $Flat"), "City  Street  House Flat", "Return the formatted text with \\n instead $, not spaces");
  });

  QUnit.test("getExecutionStatusIcon", function (assert) {
    // Assert
    assert.equal(formatter.getExecutionStatusIcon("NOT_STARTED"), "sap-icon://status-inactive", "Return inactive icon");
    assert.equal(formatter.getExecutionStatusIcon("IN_TRANSIT"), "sap-icon://status-in-process", "Return in process icon");
    assert.equal(formatter.getExecutionStatusIcon("COMPLETED"), "sap-icon://status-completed", "Return completed icon");
    assert.equal(formatter.getExecutionStatusIcon(null), "sap-icon://status-inactive", "Return inactive icon in others cases");
  });

  QUnit.test("completionFraction", function (assert) {
    // Assert
    assert.equal(formatter.completionFraction(10,100), 10, "Return inactive icon");
    assert.equal(formatter.completionFraction(56999.45,2121210.01), 2.69, "Return in process icon");
    assert.equal(formatter.completionFraction(13,89), 14.61, "Return calculated percent value");
    assert.equal(formatter.completionFraction(285,null), 0, "Return 0 in 'null' case");
    assert.equal(formatter.completionFraction(null,null), 0, "Return 0 in 'null' case");
  });

  QUnit.test("getColor", function (assert) {
    // Assert
    assert.equal(formatter.getColor("DELAYED"), ValueColor.Error, "Return neutral color");
    assert.equal(formatter.getColor("REPORTED"), ValueColor.Good, "Return critical color");
    assert.equal(formatter.getColor("OVERDUE"), ValueColor.Critical, "Return good color");
    assert.equal(formatter.getColor("PLANNED"), ValueColor.Neutral, "Return error color");
    assert.equal(formatter.getColor(null), ValueColor.Neutral, "Return error color");
  });

  QUnit.test("listCompletionRateVisibility", function (assert) {
    // Assert
    assert.equal(formatter.listCompletionRateVisibility(null,null), false, "Return false because of null entry");
    assert.equal(formatter.listCompletionRateVisibility(null,"25"), false, "Return false because of first null param");
    assert.equal(formatter.listCompletionRateVisibility("12",null), false, "Return false because of second null param");
    assert.equal(formatter.listCompletionRateVisibility("12","25"), true, "Return true because of both number entry");
  });

  QUnit.test("formatVpLocationTypeText", function (assert) {
    // Assert
    assert.equal(formatter.listCompletionRateVisibility(null), "", "Return '' because of null entry");
  });

  QUnit.test("getExecutionStatusState", function (assert) {
    var aNodes = [
      {input: "NOT_STARTED", output:ValueState.None},
      {input: "IN_TRANSIT", output:ValueState.Information},
      {input: "COMPLETED", output:ValueState.Success},
      {input: null, output:ValueState.None},
    ];

    // Assert
    aNodes.forEach(function (oNode) {
      assert.equal(
        formatter.getExecutionStatusState(oNode.input),
        oNode.output,
        formatMessage("The {0} text is got from ValueState", [oNode.output])
      );
    });
  });

  QUnit.test("formatBooleanFields", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");
    stub(formatter, "formatter").returns(formatter);
    stub(formatter.formatter, "formatYesNo").returns("yes");

    // // Assert
    assert.deepEqual(formatter.formatBooleanFields("X"), "yes", "The 'yes' text is got from i18n");
    formatter.formatter.formatYesNo.restore();

    stub(formatter.formatter, "formatYesNo").returns("no");
    assert.deepEqual(formatter.formatBooleanFields("no"), "no", "The 'no' text is got from i18n");

    assert.deepEqual(formatter.formatBooleanFields(undefined), "-", "The '-' text is for undefined value");
    assert.deepEqual(formatter.formatBooleanFields(null), "-", "The '-' text is for null value");

    formatter.getText.restore();
  });
});
