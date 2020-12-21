sap.ui.define(function () {
  "use strict";

  return {
    defaults: {
      coverage: {
        only: ["com/sap/gtt/app/sample/sst"],
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
          "com/sap/gtt/app/sample/sst": "/base/webapp",
          "com/sap/gtt/sample/ui/lib": "/base/webapp/uilib",
        },
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
