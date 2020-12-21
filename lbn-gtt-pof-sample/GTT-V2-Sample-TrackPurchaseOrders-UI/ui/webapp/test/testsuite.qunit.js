sap.ui.define(function () {
  "use strict";

  return {
    defaults: {
      coverage: {
        only: ["com/sap/gtt/app/sample/pof"],
      },
      ui5: {
        libs: [
          "sap.m",
          "sap.f",
          "sap.uxap",
          "sap.ui.comp",
          "sap.ui.fl",
          "sap.ui.table",
          "sap.ui.vbm",
          "sap.suite.ui.commons",
        ],
      },
      loader: {
        paths: {
          "com/sap/gtt/app/sample/pof": "/base/webapp",
        },
      },
      qunit: {
        version: 2,
      },
      sinon: {
        version: 4,
        qunitBridge: true,
        useFakeTimers: false,
      },
    },

    tests: {
      unitTests: {
        module: "./unit/unitTests.qunit",
        title: "QUnit Tests",
      },
      opaTests: {
        module: "./integration/AllJourneys",
        title: "OPA5 Tests",
      },
    },
  };
});
