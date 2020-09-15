sap.ui.define([
  "sap/base/strings/formatMessage",
  "sap/ui/core/ValueState",
  "com/sap/gtt/app/sample/sof/constant/ProcessStatus",
], function (formatMessage, ValueState, ProcessStatus) {
  "use strict";

  var sandbox = sinon.createSandbox();

  QUnit.module("com.sap.gtt.app.sof.constant.ProcessStatus", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("getState", function (assert) {
    var statuses = [
      { type: "EARLY", valueState: ValueState.Success },
      { type: "AS_PLANNED", valueState: ValueState.Success },
      { type: "LATE", valueState: ValueState.Warning },
      { type: "DELAYED", valueState: ValueState.Error },
      { type: "OVERDUE", valueState: ValueState.Error },
      { type: undefined, valueState: ValueState.None },
    ];

    // Assert
    statuses.forEach(function (status) {
      assert.equal(
        ProcessStatus.getState(status.type),
        status.valueState,
        formatMessage("The generic tag status of {0} status is right", [status.type])
      );
    });
  });
});
