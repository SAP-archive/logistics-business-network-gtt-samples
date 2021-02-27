sap.ui.define(
  [
    "./BaseController",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
  ],
  function (
    BaseController,
    JSONModel,
    Filter,
    FilterOperator,
    Sorter
  ) {
    "use strict";

    var PropertyPaths = Object.freeze({
      CUSTOM_FILTERS: "/customFilters",
    });

    return BaseController.extend("com.sap.gtt.app.sample.sof.controller.SalesOrderList", {
      routeName: "salesOrderList",

      initModel: function () {
        var model = new JSONModel({
          customFilters: {
            "isDelayed": {},
            "salesOrderItems.processStatus_code": {
              keys: [],
            },
            "salesOrderItems.deliveryItems.delivery.shipmentTPs.shipment.shipmentNo": {
              inputValue: "",
            },
          },
        });
        this.setModel(model, this.routeName);
      },

      initControls: function () {
        var completionRate = this.byId("completionRate");
        this.initCompletionRate(completionRate);
      },

      onSalesOrderPressed: function (oEvent) {
        var source = oEvent.getSource();
        var bindingContext = source.getBindingContext();

        this.getRouter().navTo("salesOrder", {
          id: bindingContext.getProperty("id"),
        });
      },

      onBeforeRebindTable: function (oEvent) {
        var params = oEvent.getParameter("bindingParams");
        this.addDefaultSorters(params.sorter);
        this.addCustomFilters(params.filters);
      },

      /**
       * Add the default sorter
       * @param {object[]} sorter The sorter of the odata service
       */
      addDefaultSorters: function (sorter) {
        if (sorter.length === 0) {
          var defaultSorter = new Sorter("lastChangeDateTime", true);
          sorter.push(defaultSorter);
        }
      },

      /**
       * Add custom filters such as DateTimeOffset filters
       *
       * @param {object[]} filters The filters of the odata service
       */
      addCustomFilters: function (filters) {
        var customFilters = this.getModel(this.routeName).getProperty(PropertyPaths.CUSTOM_FILTERS);
        var filterList = [];
        for (var key in customFilters) {
          if (customFilters[key] && customFilters[key].filter) {
            filterList.push(customFilters[key].filter);
          }
        }
        if (filterList.length) {
          filters.push(new Filter(filterList, true));
        }

        this.updateItemLevelFilters(filters);
      },

      updateItemLevelFilters: function (filters) {
        var materialNoControl = this.byId("smartFilterBar").getControlByKey("vMaterialNo");
        var materialDescriptionControl = this.byId("smartFilterBar").getControlByKey("vMaterialDescription");
        if (materialNoControl.getTokens().length || materialDescriptionControl.getTokens().length) {
          this.replaceFilterKeyForItemSearch(filters);
        }
      },

      replaceFilterKeyForItemSearch: function (filters) {
        filters.forEach(function (filter) {
          if (filter.aFilters) {
            this.replaceFilterKeyForItemSearch(filter.aFilters);
          }
          if (filter.sPath) {
            if (filter.sPath === "vMaterialNo") {
              filter.sPath = "salesOrderItems/materialNo";
            } else if (filter.sPath === "vMaterialDescription") {
              filter.sPath = "salesOrderItems/materialDescription";
            }
          }
        }.bind(this));
      },


      // ======================================================================
      // Events
      // ======================================================================

      /**
       * Update the filters text when filters are changed
       */
      onAssignedFiltersChanged: function () {
        var statusText = this.byId("statusText");
        var text = this.byId("smartFilterBar").retrieveFiltersWithValuesAsText();
        statusText.setText(text);
      },

      onIsDelayedChanged: function (oEvent) {
        var key = this.getModel(this.routeName).getProperty("/customFilters/isDelayed/key");

        var filters = [];
        if (key === "yes") {
          filters.push(new Filter({
            path: "isDelayed",
            operator: FilterOperator.EQ,
            value1: true,
          }));
        } else if (key === "no") {
          filters.push(new Filter({
            path: "isDelayed",
            operator: FilterOperator.EQ,
            value1: false,
          }));
          filters.push(new Filter({
            path: "isDelayed",
            operator: FilterOperator.EQ,
            value1: null,
          }));
        }

        var model = this.getModel(this.routeName);
        if (filters.length) {
          var filter = new Filter(filters, false);
          model.setProperty("/customFilters/isDelayed/filter", filter);
        } else {
          model.setProperty("/customFilters/isDelayed/filter", null);
        }

        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.fireFilterChange(oEvent);
      },

      onItemChanged: function (oEvent) {
        var source = oEvent.getSource();
        var filterKey = source.data("key");
        var newValue = oEvent.getParameter("newValue").trim();
        var filter = null;

        if (newValue) {
          filter = new Filter({
            path: filterKey.replace(/\./g, "/"),
            operator: FilterOperator.Contains,
            value1: newValue,
          });
        }

        var model = this.getModel(this.routeName);
        if (filter) {
          model.setProperty("/customFilters/".concat(filterKey).concat("/filter"), filter);
        } else {
          model.setProperty("/customFilters/".concat(filterKey).concat("/filter"), null);
        }

        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.fireFilterChange(oEvent);
      },


      // ========================================================================
      // Variant Management
      // ========================================================================

      onAfterVariantLoad: function () {
        var model = this.getModel(this.routeName);
        var smartFilterBar = this.byId("smartFilterBar");
        var data = smartFilterBar.getFilterData();
        model.setProperty(PropertyPaths.CUSTOM_FILTERS, data._CUSTOM);

        smartFilterBar.search();
      },

      onBeforeVariantSave: function (event) {
        if (event.getParameter("context") === "STANDARD") {
          this.updateCustomFilter();
        }
      },

      onBeforeVariantFetch: function () {
        this.updateCustomFilter();
      },

      updateCustomFilter: function () {
        var model = this.getModel(this.routeName);
        var customFilters = model.getProperty(PropertyPaths.CUSTOM_FILTERS);

        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.setFilterData({
          _CUSTOM: customFilters,
        });
      },
    });
  }
);
