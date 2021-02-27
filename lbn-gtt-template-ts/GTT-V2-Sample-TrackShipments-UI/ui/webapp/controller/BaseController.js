sap.ui.define(
  [
    "sap/ui/core/mvc/Controller",
    "sap/ui/core/Fragment",
    "sap/base/Log",
    "sap/base/util/isPlainObject",
    "sap/m/MessageBox",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Context",
    "../model/formatter",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../util/AsyncUtils",
  ],
  function (
    Controller,
    Fragment,
    Log,
    isPlainObject,
    MessageBox,
    JSONModel,
    Context,
    formatter,
    ServiceUtils,
    RestClient,
    AsyncUtils
  ) {
    "use strict";

    var CONSTANTS = Object.freeze({
      SLASH: "/",
    });

    function delegateComponentMethods(classInfo) {
      var methodNames = ["getRouter", "getEventBus", "getText"];

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
        this.initNavigationHandler();
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

      initViewModel: function () {
        var model = new JSONModel();
        this.setModel(model, "view");
      },

      /**
       * @abstract
       */
      initModel: function () {},

      /**
       * @abstract
       */
      subscribeEvents: function () {},

      /**
       * @abstract
       */
      initRoute: function () {
        if (this.routeName) {
          this.getRouter().getRoute(this.routeName).attachPatternMatched(this.routePatternMatched, this);
        }
      },

      initPromises: function () {
        // We use jQuery Deferred object to resolve promise object externally
        var promiseDomRendered = AsyncUtils.deferredPromise();
        this.setPromise("promiseDomRendered", promiseDomRendered);
      },

      /**
       * @abstract
       */
      initControls: function () {},

      /**
       * @abstract
       */
      initNavigationHandler: function () {},

      /**
       * @abstract
       */
      unsubscribeEvents: function () {},

      /**
       * @abstract
       */
      routePatternMatched: function () {},

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
       * Get component config
       * @returns {Object} The component config object
       */
      getComponentConfig: function () {
        var component = this.getOwnerComponent();
        return component.getManifestEntry("/sap.ui5/config");
      },

      getControlByFragment: function (fragmentId, controlId) {
        return Fragment.byId(this.createId(fragmentId), controlId);
      },


      // ============================================================
      // Location Popover
      // ============================================================

      /**
       * Attach press event to Icon in order to make it accessable
       */
      onCoordinatePressed: function () {
      },

      openLocationQuickView: function (oEvent) {
        var source = oEvent.getSource();
        this.selectedLocation = source;
        var quickView = this.locationQuickView = this.byId("locationQuickView");
        var model = quickView.getModel();
        if (!model || model.getMetadata().getName() === "sap.ui.model.odata.v2.ODataModel") {
          quickView.setModel(new JSONModel());
        }
        quickView.openBy(source);
      },

      handleLocationQuickViewAfterOpen: function () {
        var modelName = this.selectedLocation.data("modelName");
        var propertyName = this.selectedLocation.data("propertyName");
        var bindingContext = null;

        if (modelName) {
          var model = this.selectedLocation.getModel(modelName);
          bindingContext = this.selectedLocation.getBindingContext(modelName) || new Context(model, CONSTANTS.SLASH);
        } else {
          bindingContext = this.selectedLocation.getBindingContext();
        }

        this.locationQuickView.getModel().setProperty(CONSTANTS.SLASH, bindingContext.getProperty(propertyName));
      },


      // ============================================================
      // Rest Service Utilities
      // ============================================================

      createGetRequestWithId: function (name, isFreightUnit) {
        var jsonService = ServiceUtils.getDataSource("restService");
        var bindingContext = this.getView().getBindingContext();
        var id = bindingContext.getProperty("id");
        var path = isFreightUnit ? "/freightUnits/" : "/shipments/";
        var url = ServiceUtils.getUrl(jsonService.uri.concat(path).concat(name));
        var params = {};
        if (isFreightUnit) {
          params.freightUnitId = id;
        } else {
          params.shipmentId = id;
        }

        return RestClient.get(url, {
          params: params,
        }).then(function (data) {
          return data;
        }, function (error) {
          this.handleServerError(error);
          Log.error(error.data);
        }.bind(this));
      },

      // ============================================================
      // Navigation methods
      // ============================================================

      navToExternalDeliveryItemDetailPage: function (id) {
        var navObj = {
          target: {
            semanticObject: "SalesOrder",
            action: "track",
          },
        };

        if (sap.ushell && sap.ushell.Container) {
          sap.ushell.Container.getServiceAsync(
            "CrossApplicationNavigation"
          ).then(function (
            /** @type {sap.ushell.services.CrossApplicationNavigation} */
            crossAppNav
          ) {
            crossAppNav.isNavigationSupported([
              navObj,
            ]).done(function (results) {
              if (results[0].supported) {
                var href = crossAppNav.hrefForExternal(navObj);
                var url = window.location.href.split("#")[0] + href + "&/DeliveryItem(guid'" + id + "')";
                sap.m.URLHelper.redirect(url, true);
              } else {
                MessageBox.error(this.getText("crossNavigationNotSupportedMsg"));
              }
            }.bind(this));
          }.bind(this));
        }
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
      "com.sap.gtt.app.sample.sst.controller.BaseController",
      controllerClassInfo
    );
  }
);
