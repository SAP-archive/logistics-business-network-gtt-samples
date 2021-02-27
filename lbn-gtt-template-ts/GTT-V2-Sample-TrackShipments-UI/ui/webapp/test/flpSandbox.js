sap.ui.define(["sap/base/util/ObjectPath", "sap/ushell/services/Container"], function (ObjectPath) {
  "use strict";

  // define ushell config
  ObjectPath.set(["sap-ushell-config"], {
    defaultRenderer: "fiori2",
    bootstrapPlugins: {
      RuntimeAuthoringPlugin: {
        component: "sap.ushell.plugins.rta",
        config: {
          validateAppVersion: false,
        },
      },
      PersonalizePlugin: {
        component: "sap.ushell.plugins.rta-personalize",
        config: {
          validateAppVersion: false,
        },
      },
    },
    renderers: {
      fiori2: {
        componentData: {
          config: {
            enableSearch: false,
            rootIntent: "Shell-home",
          },
        },
      },
    },
    services: {
      LaunchPage: {
        adapter: {
          config: {
            groups: [
              {
                tiles: [
                  {
                    tileType: "sap.ushell.ui.tile.StaticTile",
                    properties: {
                      title: "Track Shipments",
                      subTitle: "Template",
                      icon: "sap-icon://BusinessSuiteInAppSymbols/icon-container-loading",
                      targetURL: "#Shipment-track",
                    },
                  },
                  {
                    tileType: "sap.ushell.ui.tile.StaticTile",
                    properties: {
                      title: "Track SO Fulfillment",
                      subTitle: "Template",
                      icon: "sap-icon://document",
                      targetURL: "#SalesOrder-track",
                    },
                  },
                ],
              },
            ],
          },
        },
      },
      ClientSideTargetResolution: {
        adapter: {
          config: {
            inbounds: {
              "Shipment-track": {
                semanticObject: "Shipment",
                action: "track",
                title: "Track Shipments",
                subTitle: "Template",
                icon: "sap-icon://BusinessSuiteInAppSymbols/icon-container-loading",
                signature: {
                  parameters: {},
                  additionalParameters: "allowed",
                },
                resolutionResult: {
                  applicationType: "SAPUI5",
                  additionalInformation: "SAPUI5.Component=com.sap.gtt.app.sample.sst",
                  url: sap.ui.require.toUrl("com/sap/gtt/app/sample/sst"),
                },
              },
              "SalesOrder-track": {
                semanticObject: "SalesOrder",
                action: "track",
                title: "Track SO Fulfillment",
                signature: {
                  parameters: {},
                  additionalParameters: "allowed",
                },
                resolutionResult: {
                  applicationType: "SAPUI5",
                  additionalInformation: "SAPUI5.Component=com.sap.gtt.app.sample.sof",
                  url: sap.ui.require.toUrl("com/sap/gtt/app/sample/sof"),
                },
              },
            },
          },
        },
      },
      NavTargetResolution: {
        config: {
          enableClientSideTargetResolution: true,
        },
      },
    },
  });

  var oFlpSandbox = {
    /**
     * Initializes the FLP sandbox
     * @returns {Promise} a promise that is resolved when the sandbox bootstrap has finshed
     */
    init: function () {
      // sandbox is a singleton, so we can start it only once
      if (!this._oBootstrapFinished) {
        this._oBootstrapFinished = sap.ushell.bootstrap("local");
        this._oBootstrapFinished.then(function () {
          sap.ushell.Container.createRenderer().placeAt("content");
        });
      }

      return this._oBootstrapFinished;
    },
  };

  return oFlpSandbox;
});
