sap.ui.define(
  [
    "./BaseController",
    "sap/base/util/isEmptyObject",
    "sap/ui/model/Filter",
    "sap/ui/model/FilterOperator",
    "sap/ui/model/Sorter",
    "sap/ui/generic/app/library",
    "sap/ui/generic/app/navigation/service/NavigationHandler",
  ],
  function (
    BaseController,
    isEmptyObject,
    Filter,
    FilterOperator,
    Sorter,
    GenericAppLibrary,
    NavigationHandler
  ) {
    "use strict";

    var CONSTANTS = Object.freeze({
      SLASH: "/",
      CUSTOM_FILTERS: "customFilters",
    });

    return BaseController.extend("com.sap.gtt.app.sample.sst.controller.ShipmentList", {
      routeName: "shipmentList",

      initModel: function () {
        var model = this.getModel("view");
        model.setProperty(CONSTANTS.SLASH + CONSTANTS.CUSTOM_FILTERS, {
          // deliveryNo: {
          //   inputValue: "",
          // },
        });
      },

      initNavigationHandler: function () {
        this.viewReadyPromise = new Promise(function (resolve) {
          this.fnResolveViewIsReady = resolve;
        }.bind(this));

        this.navigationHandler = new NavigationHandler(this);
        var parseNavigationPromise = this.navigationHandler.parseNavigation();
        parseNavigationPromise.done(
          function (appData, startupParameters, navType) {
            var selectionVariant = null;
            var presentationVariant = null;
            switch (navType) {
              case GenericAppLibrary.navigation.service.NavType.initial:
                break;
              case GenericAppLibrary.navigation.service.NavType.URLParams:
                // support equal filters
                if (!isEmptyObject(startupParameters)) {
                  selectionVariant = appData.oSelectionVariant;
                }
                break;
              case GenericAppLibrary.navigation.service.NavType.xAppState:
              case GenericAppLibrary.navigation.service.NavType.iAppState:
                selectionVariant = appData.oSelectionVariant;
                presentationVariant = appData.presentationVariant;
                break;
            }

            this.viewReadyPromise.then(function () {
              this.handleNavigation(navType, selectionVariant, presentationVariant);
              this.byId("smartFilterBar").search();
            }.bind(this));
          }.bind(this)
        );
      },

      routePatternMatched: function () {
        this.getEventBus().publish("tracking-timeline", "clear-map");
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
          this.refineFilters(filterData, filters);
        }
      },

      itemFilterFieldMapping: {
        deliveryId: [
          "deliveryTPs/delivery/deliveryNo",
          "freightUnitTPs/freightUnit/freightUnitItems/deliveryNo",
        ],
      },

      locationFilterFields: [
        "departureLocationId",
        "arrivalLocationId",
      ],

      /**
       * Refine the item level search and location search
       * @param {object} filterData The filter data from smart filter bar
       * @param {sap.ui.model.Filter} filters The odata request filters
       * @param {boolean} andFilter `true` if the filters are `and` filter
       */
      refineFilters: function (filterData, filters, andFilter) {
        filters.forEach(function (filter, index, list) {
          if (filter.aFilters) {
            this.refineFilters(filterData, filter.aFilters, filter.bAnd);
          } else if (this.itemFilterFieldMapping[filter.sPath]) {
            this.updateItemSearchFilters(filter, list, index, andFilter);
          } else if (this.locationFilterFields.indexOf(filter.sPath) > -1) {
            var locationFilter = this.createLocationFilter(filter.sPath, filter.oValue1);
            list.splice(index, 1, locationFilter);
          }
        }.bind(this));
      },

      updateItemSearchFilters: function (filter, filterList, index, andFilter) {
        var mapping = this.itemFilterFieldMapping[filter.sPath];
        if (typeof mapping === "string") {
          filter.sPath = this.itemFilterFieldMapping[filter.sPath];
        } else if (mapping instanceof Array) {
          var filters = mapping.map(function (path) {
            return new Filter(path, filter.sOperator, filter.oValue1, filter.oValue2);
          });
          filterList.splice(index, 1, new Filter(filters, andFilter));
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

      onInitializedSmartFilterBar: function () {
        this.fnResolveViewIsReady();
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
        var model = this.getModel("view");
        var smartFilterBar = this.byId("smartFilterBar");
        var data = smartFilterBar.getFilterData();
        model.setProperty(CONSTANTS.SLASH + CONSTANTS.CUSTOM_FILTERS, data._CUSTOM);

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
        var model = this.getModel("view");
        var customFilters = model.getProperty(CONSTANTS.SLASH + CONSTANTS.CUSTOM_FILTERS);

        var smartFilterBar = this.byId("smartFilterBar");
        smartFilterBar.setFilterData({
          _CUSTOM: customFilters,
        });
      },


      // ========================================================================
      // Cross Application Navigation
      // ========================================================================

      initiallySelectionFields: [
        "shipmentNo",
        "serviceAgentLbnId",
        "processStatus_code",
        "executionStatus_code",
        "transportationMode_code",
        "shippingType_code",
        "vehicle",
        "departureLocationId",
        "arrivalLocationId",
        "deliveryId",
      ],

      handleNavigation: function (navType, selectionVariant, presentationVariant) {
        var smartVariantManagement = this.byId("pageVariant");
        if (selectionVariant && smartVariantManagement.getCurrentVariantId()) {
          // for a standard variant, an empty string is returned.
          // if not, set standard variant.
          smartVariantManagement.setCurrentVariantId("");
        }

        if (selectionVariant) {
          var smartFilterBar = this.byId("smartFilterBar");
          var propertyNames = selectionVariant.getPropertyNames();
          propertyNames.forEach(function (name) {
            if (this.initiallySelectionFields.indexOf(name) < 0) {
              var filterItem = smartFilterBar.determineFilterItemByName(name);
              if (filterItem) {
                filterItem.setVisibleInFilterBar(true);
              }
            }
          }.bind(this));
          var uiState = new sap.ui.comp.state.UIState();
          uiState.setSelectionVariant(selectionVariant.toJSONObject());
          smartFilterBar.setUiState(uiState, {
            replace: true,
            strictMode: true,
          });

          // handle custom filters
          // this.getModel("view").setProperty("/customFilters/deliveryNo/inputValue", "abc");
          // smartFilterBar.fireFilterChange();
        }

        if (presentationVariant) {
          var stUiState = new sap.ui.comp.state.UIState();
          stUiState.setPresentationVariant(presentationVariant);
          this.byId("smartTable").setUiState(stUiState);
        }
      },
    });
  }
);
