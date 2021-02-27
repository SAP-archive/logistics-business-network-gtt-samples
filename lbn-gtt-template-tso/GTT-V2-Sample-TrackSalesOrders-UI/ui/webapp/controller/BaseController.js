sap.ui.define(
  [
    "sap/ui/core/mvc/Controller",
    "sap/ui/core/Fragment",
    "sap/base/Log",
    "sap/base/strings/formatMessage",
    "sap/base/util/isPlainObject",
    "sap/m/MessageBox",
    "sap/ui/model/json/JSONModel",
    "../model/formatter",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ],
  function (
    Controller,
    Fragment,
    Log,
    formatMessage,
    isPlainObject,
    MessageBox,
    JSONModel,
    formatter,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    function delegateComponentMethods(classInfo) {
      var methodNames = ["getRouter", "getEventBus"];

      methodNames.forEach(function (methodName) {
        classInfo[methodName] = function () {
          var component = this.getOwnerComponent();
          return component[methodName].apply(component, arguments);
        };
      });

      return classInfo;
    }

    var controllerClassInfo = {
      properiesWith3Scales: [
        "orderQuantity",
      ],

      formatter: formatter,

      onInit: function () {
        Log.info("onInit", this.getView().getId());
        this.initViewModel();
        this.initModel();
        this.subscribeEvents();
        this.initRoute();
        this.initPromises();
        this.initControls();
      },

      onExit: function () {
        this.unsubscribeEvents();
      },

      onBeforeRendering: function () {
        Log.info("onBeforeRendering", this.getView().getId());
      },

      onAfterRendering: function () {
        Log.info("onAfterRendering", this.getView().getId());

        // resolve DOM rendered promise
        var promiseDomRendered = this.getPromise("promiseDomRendered");
        if (promiseDomRendered) {
          promiseDomRendered.resolve(true);
        }
      },

      /**
       * Get startup parameters of component
       *
       * @returns {Object} The startup parameters
       */
      getStartupParameters: function () {
        var startupParameters = {};
        var component = this.getOwnerComponent();
        var componentData = component.getComponentData();

        if (componentData) {
          startupParameters = componentData.startupParameters;
        }

        return startupParameters;
      },

      /**
       * Convenience method for getting the view model by name.
       * @public
       * @param {string} [name] the model name
       * @returns {sap.ui.model.Model} the model instance
       */
      getModel: function (name) {
        return this.getView().getModel(name);
      },

      /**
       * Convenience method for setting the view model.
       * @public
       * @param {sap.ui.model.Model} model the model instance
       * @param {string} name the model name
       * @returns {sap.ui.mvc.View} the view instance
       */
      setModel: function (model, name) {
        return this.getView().setModel(model, name);
      },

      /**
       * Getter for the resource bundle.
       * @public
       * @param {string} name The name of i18n model
       * @returns {sap.ui.model.resource.ResourceModel} the resourceModel of the component
       */
      getResourceBundle: function (name) {
        var i18nModelName = name || "i18n";
        var component = this.getOwnerComponent();
        return component.getModel(i18nModelName).getResourceBundle();
      },

      /**
       * Get component config
       * @returns {Object} The component config object
       */
      getComponentConfig: function () {
        var component = this.getOwnerComponent();
        return component.getManifestEntry("/sap.ui5/config");
      },

      initViewModel: function () {
        var model = new JSONModel();
        this.setModel(model, "view");
      },

      /**
       * @abstract
       */
      initModel: function () {
      },

      /**
       * @abstract
       */
      initRoute: function () {
        if (this.routeName) {
          this.getRouter().getRoute(this.routeName).attachPatternMatched(this.routePatternMatched, this);
        }
      },

      /**
       * @abstract
       */
      subscribeEvents: function () {
      },

      /**
       * @abstract
       */
      unsubscribeEvents: function () {
      },

      /**
       * @abstract
       */
      routePatternMatched: function () {
      },

      initPromises: function () {
        // We use jQuery Deferred object to resolve promise object externally
        var promiseDomRendered = AsyncUtils.deferredPromise();
        this.setPromise("promiseDomRendered", promiseDomRendered);
      },

      getPromise: function (name) {
        var promise;

        var model = this.getModel("view");
        if (model) {
          var promises = model.getProperty("/promises");
          promise = promises[name];
        }

        return promise;
      },

      setPromise: function (name, promise) {
        var model = this.getModel("view");
        if (model) {
          var promises = model.getProperty("/promises");
          if (!promises) {
            promises = {};
            model.setProperty("/promises", promises);
          }

          promises[name] = promise;
        }
      },

      /**
       * @abstract
       */
      initControls: function () { },

      getText: function (key, params, i18nModel) {
        var resourceBundle = this.getResourceBundle(i18nModel);
        return resourceBundle.getText(key, params);
      },

      /**
       *
       * @param {sap.ui.mode.json.JSONModel} model json model
       * @param {object} params Parameters for fetching the historical events
       * @param {string} params.eventId The event id
       */
      updateHistoricalEvents: function (model, params) {
        model.setProperty("/historicalEvents", []);
        model.setProperty("/fetchingHistoricalEvents", true);

        var jsonService = ServiceUtils.getDataSource("restService");
        var url = ServiceUtils.getUrl(jsonService.uri.concat("/eventReportHistory"));
        var request = RestClient.get(url, {
          params: params || {},
        });
        request.then(function (data) {
          model.setProperty("/historicalEvents", data);
        }, function (error) {
          this.handleServerError(error);
        }.bind(this));

        AsyncUtils.finally(request, function () {
          model.setProperty("/fetchingHistoricalEvents", false);
        });
      },


      // ============================================================
      // UI5 controls
      // ============================================================

      initCompletionRate: function (control, decimalCount) {
        var floatNumberFormat = this.formatter.floatNumberFormat;
        if (decimalCount === 3) {
          floatNumberFormat = this.formatter.floatNumberFormat3Decimals;
        }
        control.addEventDelegate({
          onAfterRendering: function (oEvent) {
            var $Control = oEvent.srcControl.$();
            var valueText = $Control.find(".sapSuiteHBMCValue").text();
            var totalText = $Control.find(".sapSuiteHBMCTotal").text();
            $Control.find(".sapSuiteHBMCValue").text(floatNumberFormat.format(valueText));
            $Control.find(".sapSuiteHBMCTotal").text(floatNumberFormat.format(totalText));
          },
        });
      },

      /**
       * Attach press event to Icon in order to make it accessable
       */
      onCoordinatePressed: function () {
      },

      openLocationQuickView: function (oEvent) {
        var source = oEvent.getSource();
        this.selectedLocation = source;
        if (!this.locationQuickView) {
          Fragment.load({
            id: this.createId("locationQuickView"),
            name: "com.sap.gtt.app.sample.sof.view.fragments.LocationQuickView",
            controller: this,
          }).then(function (quickView) {
            this.locationQuickView = quickView;
            this.getView().addDependent(this.locationQuickView);
            this.locationQuickView.openBy(source);

            var model = new JSONModel();
            this.locationQuickView.setModel(model);
          }.bind(this));
        } else {
          this.locationQuickView.openBy(source);
        }
      },

      handleLocationQuickViewAfterOpen: function () {
        var propertyName = this.selectedLocation.data("propertyName");
        if (propertyName) {
          var modelName = this.selectedLocation.data("modelName");
          var bindingContext = null;
          if (modelName) {
            bindingContext = this.selectedLocation.getBindingContext(modelName);
          } else {
            bindingContext = this.selectedLocation.getBindingContext();
          }

          // update location object
          var model = this.locationQuickView.getModel();
          model.setProperty("/", bindingContext.getProperty(propertyName));
        } else {
          this.updateLocationQuickViewData();
        }
      },

      /**
       * @abstract update data for location quick view
       */
      updateLocationQuickViewData: function () {
      },

      navToShipmentApplication: function (id) {
        var crossAppNav = sap.ushell.Container.getService("CrossApplicationNavigation");
        var navObj = {
          target: {
            semanticObject: "Shipment",
            action: "track",
          },
        };
        crossAppNav.isNavigationSupported([navObj])
          .done(function (results) {
            if (results[0].supported) {
              // Open a new page
              var href = crossAppNav.hrefForExternal(navObj);
              var privappHashPrefix = "&/";
              var pattern = formatMessage("Shipment(guid''{0}'')", [id]);
              var url = window.location.href.split("#")[0] + href + privappHashPrefix + pattern;
              sap.m.URLHelper.redirect(url, true);
            } else {
              MessageBox.error(this.getText("crossNavigationNotSupportedMsg"));
            }
          }.bind(this));
      },


      // ============================================================
      // Error handler
      // ============================================================

      handleServerError: function (error) {
        var response = error.response;
        var data = response.data;
        this.handleError(data);
      },

      handleError: function (data) {
        if (!isPlainObject(data)) {
          MessageBox.error(data);
          return;
        }

        if (data.error) {
          var oError = data.error;

          var aMessages = (oError.details || []).map(function (detail) {
            return detail.message;
          });
          if (aMessages.length) {
            MessageBox.error(oError.message, {
              details: aMessages.join("<br>"),
            });
          } else {
            MessageBox.error(oError.message);
          }
        }
      },
    };

    delegateComponentMethods(controllerClassInfo);

    return Controller.extend(
      "com.sap.gtt.app.sample.sof.controller.BaseController",
      controllerClassInfo
    );
  }
);
