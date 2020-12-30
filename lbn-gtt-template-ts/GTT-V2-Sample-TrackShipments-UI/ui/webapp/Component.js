sap.ui.define(
  [
    "sap/ui/core/UIComponent",
    "sap/ui/core/IconPool",
    "sap/ui/core/theming/Parameters",
    "sap/ui/base/BindingParser",
    "./controller/ErrorHandler",
    "sap/ui/model/odata/v2/ODataModel",
    "./util/ServiceUtils",
    "./util/RestClient",
    "./util/AnnotationUtil",
    "./controller/ReportEventsDialog",
  ],
  function (
    UIComponent,
    IconPool,
    Parameters,
    BindingParser,
    ErrorHandler,
    ODataModelV2,
    ServiceUtils,
    RestClient,
    AnnotationUtil,
    ReportEventsDialog
  ) {
    "use strict";

    return UIComponent.extend("com.sap.gtt.app.sample.sst.Component", {
      metadata: {
        manifest: "json",
        handleValidation: true,
      },

      /**
       * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
       * @public
       * @override
       */
      init: function () {
        // call the base component's init function
        UIComponent.prototype.init.apply(this, arguments);
        this.setErrorHandler(new ErrorHandler(this));

        // initialize ServiceUtils
        ServiceUtils.init();

        // add business suite and tnt icons
        this.updatesstIconPool();

        // listen to theme changed event
        sap.ui.getCore().attachThemeChanged(function () {
          this.resetStylesheets();
        }.bind(this));
        this.resetStylesheets();

        var odataModel = this.getModel();
        this.enhanceReadMethod(odataModel);

        // initialize AnnotationUtil
        var metaModel = odataModel.getMetaModel();
        metaModel.loaded().then(function () {
          AnnotationUtil.init({
            metaModel: metaModel,
            annotations: odataModel.getServiceAnnotations(),
          });

          // enable routing
          this.getRouter().initialize();
        }.bind(this));

        // create dialogs
        this.reportEventsDialog = new ReportEventsDialog(this.getRootControl(), this);
      },

      exit: function () {
        // destroy dialogs
        this.reportEventsDialog.destroy();
        delete this.reportEventsDialog;
      },

      updatesstIconPool: function () {
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
        var config = this.getMetadata().getConfig();
        config.css.forEach(function (resource) {
          var stylesheetId = resource.id;
          var stylesheetUrl = resource.uri;
          this.lessifyCSS(stylesheetId, stylesheetUrl);
        }.bind(this));
      },

      lessifyCSS: function (id, stylesheetUrl) {
        var stylesheetId = id + "-less-css";
        var stylesheetNode = document.getElementById(stylesheetId);

        if (!stylesheetNode) {
          stylesheetNode = document.createElement("style");
          stylesheetNode.id = stylesheetId;
          stylesheetNode.type = "text/css";
          var headerElement = document.getElementsByTagName("head")[0];
          headerElement.appendChild(stylesheetNode);
          stylesheetNode = document.getElementById(stylesheetId);
        }

        stylesheetNode.innerHTML = "";

        // load stylesheet
        var cssUrl = ServiceUtils.getUrl(stylesheetUrl);
        RestClient.get(cssUrl).then(function (response) {
          var stylesheetText = response;

          if (stylesheetText !== null && stylesheetText.length !== 0) {
            // replace stylesheet content placeholders
            stylesheetText = stylesheetText.replace(/@([\w]+)/g, function (match, parameterName) {
              var parameterValue = Parameters.get(parameterName);

              if (parameterValue === null) {
                parameterValue = "@" + parameterName;
              }

              return parameterValue;
            });
          }

          stylesheetNode.innerHTML = stylesheetText;
        });
      },

      enhanceReadMethod: function (odataModel) {
        var codeListEntitySetList = [
          "ExecutionStatus",
          "ProcessStatus",
          "ShippingType",
          "TransportationMode",
          "TransportMeansStandardCode",
          "TrafficDirection",
        ];
        odataModel.read = function (sPath, mParameters) {
          var reg = /\/([a-zA-Z0-9]*)(\(.*\))?/;
          var matches = sPath.match(reg);
          var entitySet = matches && matches[1];
          var urlParameters = mParameters.urlParameters;
          if (entitySet && codeListEntitySetList.indexOf(entitySet) > -1) {
            urlParameters.push("$expand=localized");
          }

          return ODataModelV2.prototype.read.call(this, sPath, mParameters);
        };
      },

      /**
       * Sets the component's error handler instance.
       *
       * @param {sap.lbn.uilib.abstracts.helpers.ErrorHandler} errorHandler Error handler instance
       * @private
       */
      setErrorHandler: function (errorHandler) {
        this.errorHandler = errorHandler;
      },

      /**
       * Returns the component's error handler instance.
       *
       * @return {sap.lbn.uilib.controller.ErrorHandler} Error handler instance
       * @public
       */
      getErrorHandler: function () {
        return this.errorHandler;
      },

      /**
       * Getter for the resource bundle.
       * @public
       * @param {string} name The name of i18n model
       * @returns {sap.ui.model.resource.ResourceModel} the resourceModel of the component
       */
      getResourceBundle: function (name) {
        var i18nModelName = name || "i18n";
        return this.getModel(i18nModelName).getResourceBundle();
      },

      getText: function (key, params, i18nModel)  {
        var resourceBundle = this.getResourceBundle(i18nModel);
        return resourceBundle.getText(key, params);
      },

      getPropertyLabelText: function (propertyName, entitySet) {
        var label = AnnotationUtil.getPropertyLabel(propertyName, entitySet);
        var bindingInfo = BindingParser.simpleParser(label);
        if (!bindingInfo) {
          return label;
        }

        return this.getText(bindingInfo.path, null, bindingInfo.model);
      },
    });
  }
);
