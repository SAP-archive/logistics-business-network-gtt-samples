sap.ui.define([
  "sap/base/strings/formatMessage",
  "sap/ui/core/ValueState",
  "com/sap/gtt/app/sample/sst/constant/ProcessStatus",
], function (formatMessage, ValueState, ProcessStatus) {
  "use strict";

  var sandbox = sinon.createSandbox();

  QUnit.module("com.sap.gtt.app.sst.constant.ProcessStatus", {
    beforeEach: function () {
    },
    afterEach: function () {
      sandbox.restore();
    },
  });

  QUnit.test("getState", function (assert) {
    var statuses = [
      { type: "AS_PLANNED", valueState: ValueState.Information },
      { type: "EARLY", valueState: ValueState.Warning },
      { type: "LATE", valueState: ValueState.Warning },
      { type: "OVERDUE", valueState: ValueState.Warning },
      { type: "DELAYED", valueState: ValueState.Error },
      { type: undefined, valueState: ValueState.Information },
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
