sap.ui.define([
  "sap/ui/core/UIComponent",
  "sap/ui/core/IconPool",
  "sap/ui/core/theming/Parameters",
  "./util/ServiceUtils",
  "./util/RestClient",
  "./util/AnnotationUtil",
  "com/sap/gtt/app/sample/pof/controller/mixins/TextHelper.mixin",
  "com/sap/gtt/app/sample/pof/controller/mixins/ErrorHandler.mixin",
], function (UIComponent, IconPool, Parameters, ServiceUtils, RestClient, AnnotationUtil, TextHelper, ErrorHandler) {
  "use strict";

  return UIComponent.extend("com.sap.gtt.app.sample.pof.Component", {
    metadata: {
      manifest: "json",
    },

    /**
     * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
     * @public
     * @override
     */
    init: function () {
      // call the base component's init function
      UIComponent.prototype.init.apply(this, arguments);
      // initialize
      ServiceUtils.init({
        component: this,
      });
      TextHelper.setComponent(this);
      ErrorHandler.setComponent(this);
      ErrorHandler.initErrorHandler();

      // add business suite and tnt icons
      this.updatepofIconPool();
      // listen to theme changed event
      sap.ui.getCore().attachThemeChanged(function () {
        this.resetStylesheets();
      }.bind(this));

      var oMetaModel = this.getModel().getMetaModel();
      oMetaModel.loaded()
        .then(function () {
          AnnotationUtil.init({metaModel: oMetaModel});
          // enable routing
          this.getRouter().initialize();
        }.bind(this));
    },

    updatepofIconPool: function () {
      IconPool.addIcon("icon-truck-driver", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0df");
      IconPool.addIcon("icon-truck-load", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0e8");
      IconPool.addIcon("icon-box-truck", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0ce");
      IconPool.addIcon("icon-truck-unload", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0e9");
      IconPool.addIcon("icon-products", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e073");
      IconPool.addIcon("icon-box-truck-empty", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0cf");
      IconPool.addIcon("icon-vessel", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e105");
      IconPool.addIcon("icon-warehouse", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0dc");
      IconPool.addIcon("icon-outbound-delivery", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e07d");
      IconPool.addIcon("icon-ship", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0de");
      IconPool.addIcon("icon-container", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e070");
      IconPool.addIcon("icon-container-loading", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e071");
      IconPool.addIcon("internal-block-diagram", "SAP-icons-TNT", "SAP-icons-TNT", "e059");
    },

    getComponentName: function () {
      return this.getManifestObject().getComponentName();
    },

    resetStylesheets: function () {
      var oMetadataConfig = this.getMetadata().getConfig();
      this.lessifyCSS(oMetadataConfig.css.id, oMetadataConfig.css.uri);
    },

    lessifyCSS: function (sId, sStylesheetUrl) {
      var sStylesheetId = sId + "-less-css";
      var oStylesheetNode = document.getElementById(sStylesheetId);

      if (!oStylesheetNode) {
        oStylesheetNode = document.createElement("style");
        oStylesheetNode.id = sStylesheetId;
        oStylesheetNode.type = "text/css";
        var oHeaderElement = document.getElementsByTagName("head")[0];
        oHeaderElement.appendChild(oStylesheetNode);
        oStylesheetNode = document.getElementById(sStylesheetId);
      }

      oStylesheetNode.innerHTML = "";

      // load stylesheet
      var sCssUrl = ServiceUtils.getUrl(sStylesheetUrl);
      RestClient.get(sCssUrl).then(function (response) {
        var sStylesheetText = response;

        if (sStylesheetText !== null && sStylesheetText.length !== 0) {
          // replace stylesheet content placeholders
          sStylesheetText = sStylesheetText.replace(/@([\w]+)/g, function (match, parameterName) {
            var sParameterValue = Parameters.get(parameterName);

            if (sParameterValue === null) {
              sParameterValue = "@" + sParameterValue;
            }
            return sParameterValue;
          });
        }
        oStylesheetNode.innerHTML = sStylesheetText;
      });
    },
  });
});
