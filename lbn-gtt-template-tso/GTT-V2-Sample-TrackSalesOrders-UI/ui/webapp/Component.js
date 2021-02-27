sap.ui.define(
  [
    "sap/ui/core/UIComponent",
    "sap/ui/core/IconPool",
    "sap/ui/core/theming/Parameters",
    "sap/ui/base/BindingParser",
    "./controller/ErrorHandler",
    "./util/ServiceUtils",
    "./util/RestClient",
  ],
  function (
    UIComponent,
    IconPool,
    Parameters,
    BindingParser,
    ErrorHandler,
    ServiceUtils,
    RestClient
  ) {
    "use strict";

    return UIComponent.extend("com.sap.gtt.app.sample.sof.Component", {
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
        this.setErrorHandler(new ErrorHandler(this));

        // initialize
        ServiceUtils.init({
          component: this,
        });

        // add business suite and tnt icons
        this.updateSOFIconPool();

        // listen to theme changed event
        sap.ui.getCore().attachThemeChanged(function () {
          this.resetStylesheets();
        }.bind(this));
        this.resetStylesheets();

        /** @type {sap.ui.model.odata.v2.ODataModel} */
        var odataModel = this.getModel();

        // init the property label model
        odataModel.annotationsLoaded().then(function (annotations) {
          this.initPropertyLabelModel(annotations.annotations);
        }.bind(this));

        // enable routing
        this.getRouter().initialize();
      },

      updateSOFIconPool: function () {
        IconPool.addIcon("icon-truck-load", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0e8");
        IconPool.addIcon("icon-box-truck", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0ce");
        IconPool.addIcon("icon-truck-unload", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0e9");
        IconPool.addIcon("icon-products", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e073");
        IconPool.addIcon("icon-box-truck-empty", "BusinessSuiteInAppSymbols", "BusinessSuiteInAppSymbols", "e0cf");
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

      /**
       * Init the property label JSONModel named "label"
       *
       * @param {object} annotations Annotation object
       *
       * @example {label>/Product/id}  // {label>/<EntitySetName>/<PropertyName>}
       */
      initPropertyLabelModel: function (annotations) {
        var propertyAnnotations = annotations.propertyAnnotations;

        /** @type {sap.ui.model.json.JSONModel} */
        var labelModel = this.getModel("label");
        var labels = {};

        Object.keys(propertyAnnotations).forEach(function (entitySetFullName) {
          var entitySetName = entitySetFullName.split(".").pop();
          var entitySet = propertyAnnotations[entitySetFullName];

          if (!labels[entitySetName]) {
            labels[entitySetName] = {};
          }

          Object.keys(entitySet).forEach(function (propertyName) {
            var labelAnnotation = entitySet[propertyName]["com.sap.vocabularies.Common.v1.Label"];

            if (labelAnnotation) {
              var bindingInfo = BindingParser.simpleParser(labelAnnotation.String);
              labels[entitySetName][propertyName] = this.getModel(bindingInfo.model).getResourceBundle().getText(bindingInfo.path);
            } else {
              labels[entitySetName][propertyName] = propertyName;
            }
          }, this);
        }, this);

        labelModel.setData(labels);
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
    });
  }
);
