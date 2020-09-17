sap.ui.define([
  "com/sap/gtt/app/sample/sof/model/formatter",
  "sap/base/strings/formatMessage",
  "sap/m/ValueColor",
  "sap/ui/core/ValueState",
], function (formatter, formatMessage, ValueColor, ValueState) {
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

  QUnit.module("com.sap.gtt.app.sof.model.formatter", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("processStatusText, processStatusColor", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");

    // Act
    var text = formatter.processStatusText("status");
    var color = formatter.processStatus.getColor("status");

    // Assert
    assert.equal(text, "fakeText", "The text is right");
    assert.ok(formatter.getText.calledWith("CO_ProcessStatus_status_NAME", null, "@i18n"), "The status text is got from @i18n");
    assert.equal(color, ValueColor.Good, "The status is right");
  });

  QUnit.test("processStatusIcon, processStatusState", function (assert) {
    // Act
    var icon = formatter.processStatus.getIcon("status");
    var state = formatter.processStatus.getState("status");

    // Assert
    assert.equal(icon, "sap-icon://process", "The icon is right");
    assert.equal(state, "None", "The state is right");
  });

  QUnit.test("milestoneSummary", function (assert) {
    // Act
    var summary = formatter.milestoneSummary("node");
    var summaryWithoutNode = formatter.milestoneSummary();

    // Assert
    assert.equal(summary, "<strong>100</strong> / 200 items", "The milestone summary is right");
    assert.equal(summaryWithoutNode, "", "The milestone summary without node is right");
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

  QUnit.test("networkGraphGroupNodeIcon", function (assert) {
    var nodes = [
      { group: 1, icon: "sap-icon://my-sales-order" },
      { group: 2, icon: "sap-icon://sales-order-item" },
      { group: 3, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-box-truck" },
      { group: 4, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-outbound-delivery" },
      { group: 5, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-container-loading" },
      { group: 6, icon: "sap-icon://BusinessSuiteInAppSymbols/icon-container" },
      { group: undefined, icon: "" },
    ];

    // Assert
    nodes.forEach(function (node) {
      assert.equal(
        formatter.networkGraphGroupNodeIcon(node.group),
        node.icon,
        formatMessage("The icon of group {0} is right", [node.group])
      );
    });
  });

  QUnit.test("networkGraphNodeTitle", function (assert) {
    // Arrange
    stub(formatter, "getText").returns("fakeText");

    var nodes = [
      { group: 1, i18n: "salesOrder" },
      { group: 2, i18n: "salesOrderItem" },
      { group: 3, i18n: "deliveryItem" },
      { group: 4, i18n: "delivery" },
      { group: 5, i18n: "shipment" },
      { group: 6, i18n: "resource" },
    ];

    // Act
    nodes.forEach(function (node) {
      formatter.networkGraphNodeTitle(node.group);
    });

    // Assert
    nodes.forEach(function (node, index) {
      assert.equal(
        formatter.getText.args[index],
        node.i18n,
        formatMessage("The \"{0}\" text is got from i18n", [node.i18n])
      );
    });
  });
});
