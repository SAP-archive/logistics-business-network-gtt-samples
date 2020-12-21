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

    return BaseController.extend("com.sap.gtt.app.sample.sst.controller.ShipmentList", {
      routeName: "shipmentList",

      initModel: function () {
        var model = new JSONModel({
          customFilters: {},
        });
        this.setModel(model, this.routeName);
      },

      routePatternMatched: function () {
        this.getEventBus().publish("tracking-timeline", "clear-map");
        this.getEventBus().publish("tracking-timeline", "cancel-map-request");
      },

      onShipmentPressed: function (oEvent) {
        var source = oEvent.getSource();
        var bindingContext = source.getBindingContext();

        this.getRouter().navTo("shipment", {
          id: bindingContext.getProperty("id"),
        });
      },

      onBeforeRebindTable: function (oEvent) {
        var params = oEvent.getParameter("bindingParams");
        this.addDefaultSorters(params.sorter);
        this.updateFilters(params.filters);
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

      updateFilters: function (filters) {
        var filterData = this.byId("smartFilterBar").getFilterData();
        if (filterData.departureLocationId || filterData.arrivalLocationId
          || filterData.deliveryId) {
          this.refineFilters(filters);
        }
      },

      itemFilterFieldsMappings: {
        deliveryId: "deliveryTPs/delivery/deliveryNo",
      },

      locationFilterFields: [
        "departureLocationId",
        "arrivalLocationId",
      ],

      /**
       * Refine the item level search and location search
       * @param {sap.ui.model.Filter} filters The odata request filters
       */
      refineFilters: function (filters) {
        var filterData = this.byId("smartFilterBar").getFilterData();
        if (filterData.departureLocationId || filterData.arrivalLocationId
          || filterData.deliveryId) {
          filters.forEach(function (filter, index, list) {
            if (filter.aFilters) {
              this.refineFilters(filter.aFilters);
            } else if (this.itemFilterFieldsMappings[filter.sPath]) {
              filter.sPath = this.itemFilterFieldsMappings[filter.sPath];
            } else if (this.locationFilterFields.indexOf(filter.sPath) > -1) {
              var locationFilter = this.createLocationFilter(filter.sPath, filter.oValue1);
              list.splice(index, 1, locationFilter);
            }
          }.bind(this));
        }
      },

      /**
       * Create a combined filter for location
       * Location Alternative Key: xri://sap.com/id:LBN#10010001006:QM7CLNT910:Location:Customer:0000010105
       * The new filter is a combined one by 3 filters
       * sourceSystem eq 'QM7CLNT910'
       * departureLocationType eq 'Customer' or arrivalLocationType eq 'Customer'
       * departureLocationId eq '0000010105' or arrivalLocationId eq '0000010105'
       * @param {string} filterKey The filter key `departureLocationId` or `arrivalLocationId`
       * @param {string} locationAltKey The location alternative key
       * @returns {sap.ui.model.Filter} One combined filter
       */
      createLocationFilter: function (filterKey, locationAltKey) {
        var items = locationAltKey.split(":").slice(-4);
        var locationIdFilter = new Filter(filterKey, FilterOperator.EQ, items.pop());
        var locationTypeFilter = new Filter(filterKey.replace("Id", "Type_code"), FilterOperator.EQ, items.pop());
        var sourceSystemFilter = new Filter("logicalSystem", FilterOperator.EQ, items.shift());

        return new Filter([locationIdFilter, locationTypeFilter, sourceSystemFilter], true);
      },


      // ======================================================================
      // Events
      // ======================================================================

      onInitSmartFilterBar: function () {
        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.getControlByKey("departureLocationId").setValueHelpOnly(true);
        smartFilterBar.getControlByKey("arrivalLocationId").setValueHelpOnly(true);
      },

      /**
       * Update the filters text when filters are changed
       */
      onAssignedFiltersChanged: function () {
        var statusText = this.byId("statusText");
        var text = this.byId("smartFilterBar").retrieveFiltersWithValuesAsText();
        statusText.setText(text);
      },


      // ========================================================================
      // Variant Management
      // ========================================================================

      onAfterVariantLoad: function () {
        var model = this.getModel(this.routeName);
        var smartFilterBar = this.byId("smartFilterBar");
        var data = smartFilterBar.getFilterData();
        model.setProperty("/customFilters", data._CUSTOM);
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
        var customFilters = model.getProperty("/customFilters");

        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.setFilterData({
          _CUSTOM: customFilters,
        });
      },
    });
  }
);
