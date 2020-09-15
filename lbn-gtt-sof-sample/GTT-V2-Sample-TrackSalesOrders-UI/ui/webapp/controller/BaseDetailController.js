sap.ui.define(
  [
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/ui/Device",
  ],
  function (
    BaseController,
    JSONModel,
    Device
  ) {
    "use strict";

    var controllerClassInfo = {

      initModel: function () {
        var model = new JSONModel();
        this.setModel(model, this.routeName);
      },

      /**
       * Binding the view and free the busy state
       * @param {string} entitySetKey The key of the entityset like EntitySet('id')
       */
      bindView: function (entitySetKey) {
        var model = this.getModel(this.routeName);
        this.setViewBusy(model);

        var view = this.getView();
        var parameters = {};
        var expandList = this.getExpandList();
        if (expandList.length) {
          parameters.expand = expandList.join(",");
        }

        view.bindElement({
          path: "/" + entitySetKey,
          parameters: parameters,
          events: {
            change: this.onBindingChange.bind(this),
          },
        });
      },

      getExpandList: function () {
        return [];
      },

      /**
       * Do some logic when the binding is changed.
       * If binding context is null, show 'Not Found' page.
       */
      onBindingChange: function () {
        this.setViewFree(this.getModel(this.routeName));

        var bindingContext = this.getView().getBindingContext();
        if (!bindingContext) {
          // If there is no binding context, show 'Not Found' page.
          this.getRouter().getTargets().display("notFound");
        } else {
          window.setTimeout(this.scrollToHeader.bind(this), 500);
          this.updateView();
        }
      },

      /**
       * @abstract
       */
      updateView: function () {
      },

      /**
       * Process fulfillment status for comparison chart
       * @param {object[]} items The items to be processed
       * @returns {object[]} The status set
       */
      getFulfillmentStatusSet: function (items) {
        var fulFillmentStatus = {
          "EARLY": 0,
          "AS_PLANNED": 0,
          "LATE": 0,
          "DELAYED": 0,
          "OVERDUE": 0,
        };

        items.forEach(function (item) {
          var processStatusCode = item.processStatus_code;
          fulFillmentStatus[processStatusCode]++;
        });

        var fulFillmentStatusSet = [];
        Object.keys(fulFillmentStatus).forEach(function (status) {
          fulFillmentStatusSet.push({
            status: status,
            value: fulFillmentStatus[status],
          });
        });

        return fulFillmentStatusSet;
      },

      /**
       * Add loading indicator for the view - busy state
       * @param {sap.ui.model.json.JSONModel} model The json model
       */
      setViewBusy: function (model) {
        var originalBusyDelay = this.getView().getBusyIndicatorDelay();
        model.setProperty("/busy", true);
        model.setProperty("/delay", originalBusyDelay);
      },

      /**
       * Remove loading indicator for the view - not busy state
       * @param {sap.ui.model.json.JSONModel} model The json model
       */
      setViewFree: function (model) {
        var originalBusyDelay = this.getView().getBusyIndicatorDelay();
        model.setProperty("/busy", false);
        model.setProperty("/delay", originalBusyDelay);
      },

      scrollToHeader: function () {
        var objectPageLayout = this.byId("objectPageLayout");
        var scrollEnablement = objectPageLayout.getScrollDelegate();
        scrollEnablement.scrollTo(0, 0, 100); // (x, y, time)
      },

      /**
       * Refresh the subsection views
       *
       * @param {string} subSectionViewId The sub section id
       */
      refreshSubSection: function (subSectionViewId) {
        this.byId(subSectionViewId).getController().refresh();
      },

      navToSalesOrderList: function () {
        this.getRouter().navTo("salesOrderList");
      },

      navToSalesOrder: function (oEvent) {
        this.getRouter().navTo("salesOrder", {
          id: oEvent.getSource().data("salesOrderId"),
        });
      },

      navToSalesOrderItem: function (oEvent) {
        var source = oEvent.getSource();
        this.getRouter().navTo("salesOrderItem", {
          id: source.data("salesOrderItemId"),
          params: {
            salesOrderId: source.data("salesOrderId"),
          },
        });
      },


      // ============================================================
      // UI5 controls
      // ============================================================

      /**
       * @param {string[]} genericTagsIds IDs of Generic Tags
       */
      initGenericTags: function (genericTagsIds) {
        genericTagsIds.forEach(function (id) {
          var genericTag = this.byId(id);
          genericTag.addEventDelegate({
            onAfterRendering: function () {
              genericTag.$().removeAttr("tabindex");
              genericTag.$().attr("role", "kpi");
            },
          });
        }, this);
      },

      /**
       * @param {string} routeName Route name of controller
       */
      registerEvents: function (routeName) {
        var model = this.getModel(routeName);
        function _sizeChanged(mParams) {
          switch (mParams.name) {
            case "Phone":
              model.setProperty("/genericTagPriority", sap.m.OverflowToolbarPriority.AlwaysOverflow);
              break;
            case "Tablet":
            case "Desktop":
              model.setProperty("/genericTagPriority", sap.m.OverflowToolbarPriority.NeverOverflow);
              break;
          }
        }

        // Register an event handler to changes of the screen size
        Device.media.attachHandler(_sizeChanged, null, Device.media.RANGESETS.SAP_STANDARD);

        // Do some initialization work based on the current size
        _sizeChanged(Device.media.getCurrentRange(Device.media.RANGESETS.SAP_STANDARD));
      },
    };

    return BaseController.extend(
      "com.sap.gtt.app.sample.sof.controller.BaseDetailController",
      controllerClassInfo
    );
  }
);
