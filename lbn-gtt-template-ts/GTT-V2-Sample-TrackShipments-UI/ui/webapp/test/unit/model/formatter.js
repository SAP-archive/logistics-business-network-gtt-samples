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

  QUnit.test("ISODate, ISODateTime", function (assert) {
    var isoDate = new ISODate();
    var isoDateTime = new ISODateTime();

    var date = isoDate.formatValue("2011-10-02", "string");
    var datetime = isoDateTime.formatValue("2011-10-02T12:20:30.000Z", "string");

    assert.ok(date, "The formatted date is got");
    assert.ok(datetime, "The formatted datetime is got");
  });

  QUnit.test("geoCoordinatesTooltip", function (assert) {
    // Arrange
    stub(formatter, "getText");

    // Act
    formatter.geoCoordinatesTooltip(null, null);

    // Assert
    assert.ok(formatter.getText.calledWith("geoCoordinatesMissing"), "Geo coordinates is missing");
  });

  QUnit.test("stopTooltip, stopIcon, stopPosition", function (assert) {
    // Arrange
    stub(formatter, "getText").withArgs("sourceLocation").returns("sourceLocation");

    // Act
    var tooltip = formatter.stopTooltip("locId", true, "locId", "locId2", "descrption");
    var icon = formatter.stopIcon("locId", true, "locId");
    var pos = formatter.stopPosition(null);

    // Assert
    assert.ok(tooltip === "sourceLocation: descrption", "The tooltip is right");
    assert.ok(icon === "sap-icon://arrow-top", "the source location icon is right");
    assert.ok(pos === "", "The position is right");
  });

  QUnit.test("eventTypeText", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("ET_type_DESCR");

    // Act
    var text1 = formatter.eventTypeText(null);
    var text2 = formatter.eventTypeText("type");

    // Asssert
    assert.ok(text1 === "", "The text is right");
    assert.ok(text2 === "type", "The text is right");
  });

  QUnit.test("eventStatusText", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("unplanned").returns("unplanned");
    // Act
    var text1 = formatter.eventStatusText(null);
    var text2 = formatter.eventStatusText("UNPLANNED");

    // Assert
    assert.ok(text1 === "", "The text is right");
    assert.ok(text2 === "unplanned", "The text is right");
  });
});
