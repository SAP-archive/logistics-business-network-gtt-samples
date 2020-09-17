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
                      title: "Sales Orders Fulfillment",
                      subTitle: "Sales Orders Fulfillment",
                      icon: "sap-icon://document",
                      targetURL: "#SalesOrders-track",
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
              "SalesOrders-track": {
                semanticObject: "SalesOrders",
                action: "track",
                title: "Sales Orders Fulfillment",
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
