sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/base/Log",
  "sap/ui/model/Sorter",
  "sap/ui/base/BindingParser",
  "sap/ui/core/format/DateFormat",
  "com/sap/gtt/app/sample/pof/model/formatter",
  "com/sap/gtt/app/sample/pof/util/Constants",
  "com/sap/gtt/app/sample/pof/util/AnnotationUtil",
  "com/sap/gtt/app/sample/pof/controller/mixins/ViewHelper.mixin",
  "com/sap/gtt/app/sample/pof/controller/mixins/ModelHelper.mixin",
  "com/sap/gtt/app/sample/pof/controller/mixins/ErrorHandler.mixin",
  "com/sap/gtt/app/sample/pof/controller/mixins/PromiseHelper.mixin",
  "com/sap/gtt/app/sample/pof/controller/mixins/ComponentHelper.mixin",
  "sap/ui/Device",
  "sap/ui/core/format/NumberFormat",
], function (Controller, Log, Sorter, BindingParser, DateFormat, formatter, Constants, AnnotationUtil,
  ViewHelper, ModelHelper, ErrorHandler, PromiseHelper, ComponentHelper, Device, NumberFormat) {
  "use strict";

  var oControllerClassInfo = {
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
      var oPromiseDomRendered = this.getPromise("promiseDomRendered");
      if (oPromiseDomRendered) {
        oPromiseDomRendered.resolve(true);
      }
    },

    /**
     * Do some logic when the binding is changed.
     * If binding context is null, show 'Not Found' page.
     */
    onBindingChange: function () {
      var oBindingContext = this.getView().getBindingContext();
      if (!oBindingContext) {
        // If there is no binding context, show 'Not Found' page.
        this.getRouter().getTargets().display("notFound");
      } else {
        this.updateView();
      }
    },

    /**
    * Add the default sorter.
    * @param {object[]} oSorter The sorter of the odata service
    * @param {string[]} aSorterProperties sorter should considers this properties
    */
    addDefaultSorters: function (oSorter, aSorterProperties) {
      if (oSorter.length === 0) {
        aSorterProperties.forEach(function (sSortProperty) {
          oSorter.push(new Sorter(sSortProperty, false));
        });
      }
    },

    onDataRequested: function () {
      this.setViewBusy(this.getModel(this.routeName));
      this.updateLastUpdatedAtTime();
    },

    /**
     * Update last updated time
     */
    updateLastUpdatedAtTime: function () {
      var oModel = this.getModel("view");
      if (!oModel.getProperty(Constants.LAST_UPDATE_PATH)) {
        oModel.setProperty(Constants.LAST_UPDATE_PATH, {});
      }

      var oDateTimeInstance = DateFormat.getTimeInstance({
        style: "medium",
        UTC: false,
      });

      oModel.setProperty(
        Constants.LAST_UPDATE_PATH + this.getView().getElementBinding().getPath(),
        this.getText("lastUpdatedAt", [
          oDateTimeInstance.format(new Date()),
        ])
      );
    },

    onDataReceived: function () {
      this.setViewFree(this.getModel(this.routeName));
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

    /**
     * @abstract
     */
    initControls: function () {},

    getText: function (sKey, aParams, sI18nModel)  {
      var oResourceBundle = this.getResourceBundle(sI18nModel);
      return oResourceBundle.getText(sKey, aParams);
    },

    getControlFromFragment: function (sFragmentId, sControlId) {
      return this.byId(this.getControlFragmentId(sFragmentId, sControlId));
    },

    getControlFragmentId: function (sFragmentId, sControlId) {
      return sap.ui.core.Fragment.createId(sFragmentId, sControlId);
    },

    getPropertyLabelText: function (sPropertyName, oEntitySet) {
      var sLabel = AnnotationUtil.getPropertyLabel(sPropertyName, oEntitySet);
      var oBindingInfo = BindingParser.simpleParser(sLabel);
      if (!oBindingInfo) {
        return sLabel;
      }

      return this.getText(oBindingInfo.path, null, oBindingInfo.model);
    },

    /**
     *Format text value for Completion rate
     * @param {Object[]} aControls array of controls
     * @param {Number} iIndex number of decimals after dot
     */
    initCompletionRate: function (aControls, iIndex) {
      var oFloatNumberFormat;
      if(iIndex === Constants.TWO_DECIMALS_AFTER_DOT) {
        oFloatNumberFormat = NumberFormat.getFloatInstance({
          decimals: 2,
          groupingEnabled: true,
        }, sap.ui.getCore().getConfiguration().getLocale());
      } else {
        oFloatNumberFormat = NumberFormat.getFloatInstance({
          decimals: 3,
          groupingEnabled: true,
        }, sap.ui.getCore().getConfiguration().getLocale());

      }

      aControls.forEach(function (oControl) {
        oControl.addEventDelegate({
          onAfterRendering: function (oEvent) {
            var $Control = oEvent.srcControl.$();
            var oChart = oEvent.srcControl;
            var oChartItem = oChart.getItems()[0];
            var sFractionText = oChartItem.getFraction();
            var sTotalText = oChart.getTotal();

            $Control.find(".sapSuiteHBMCValue").text(oFloatNumberFormat.format(sFractionText));
            $Control.find(".sapSuiteHBMCTotal").text(oFloatNumberFormat.format(sTotalText));
          },
        });
      });
    },
    /**
       * @param {string[]} aGenericTagsIds IDs of Generic Tags
       */
    initGenericTags: function (aGenericTagsIds) {
      aGenericTagsIds.forEach(function (sId) {
        var oGenericTag = this.byId(sId);
        oGenericTag.addEventDelegate({
          onAfterRendering: function () {
            oGenericTag.$().removeAttr("tabindex");
            oGenericTag.$().attr("role", "kpi");
          },
        });
      }, this);
    },

    /**
     * Register an event handler to changes of the screen size,
     * and set Overflow behavior to generic tags in headers
     * @param {string} sRouteName Route name of controller
     */
    registerEvents: function (sRouteName) {
      var oModel = this.getModel(sRouteName);
      function _sizeChanged(mParams) {
        switch (mParams.name) {
          case Constants.DEVICE_TYPE.PHOHE:
            oModel.setProperty("/genericTagPriority", sap.m.OverflowToolbarPriority.AlwaysOverflow);
            break;
          case Constants.DEVICE_TYPE.TABLET:
          case Constants.DEVICE_TYPE.DESKTOP:
            oModel.setProperty("/genericTagPriority", sap.m.OverflowToolbarPriority.NeverOverflow);
            break;
        }
      }

      Device.media.attachHandler(_sizeChanged, null, Device.media.RANGESETS.SAP_STANDARD);

      // Do some initialization work based on the current size
      _sizeChanged(Device.media.getCurrentRange(Device.media.RANGESETS.SAP_STANDARD));
    },

    // ============================================================
    // Error handler
    // ============================================================
  };

  ComponentHelper.delegateComponentMethods(oControllerClassInfo);

  return Controller.extend(
    "com.sap.gtt.app.sample.pof.controller.BaseController",
    jQuery.extend({}, oControllerClassInfo, ViewHelper, ModelHelper, ErrorHandler, PromiseHelper, ComponentHelper)
  );
});
