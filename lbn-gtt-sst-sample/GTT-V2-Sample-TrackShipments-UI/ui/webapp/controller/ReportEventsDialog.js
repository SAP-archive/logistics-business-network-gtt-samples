sap.ui.define(
  [
    "sap/ui/base/ManagedObject",
    "sap/ui/core/Fragment",
    "sap/ui/layout/form/FormElement",
    "sap/m/Input",
    "sap/m/Select",
    "sap/ui/core/Item",
    "sap/m/DatePicker",
    "sap/m/DateTimePicker",
    "sap/m/MessageBox",
    "sap/ui/core/ValueState",
    "sap/ui/model/json/JSONModel",
    "sap/ui/model/BindingMode",
    "../model/formatter",
    "../util/ServiceUtils",
    "../util/RestClient",
    "../constant/CustomField",
  ],
  function (
    ManagedObject,
    Fragment,
    FormElement,
    Input,
    Select,
    Item,
    DatePicker,
    DateTimePicker,
    MessageBox,
    ValueState,
    JSONModel,
    BindingMode,
    formatter,
    ServiceUtils,
    RestClient,
    CustomField
  ) {
    "use strict";

    var FORM_MODEL_NAME = "eventDetail";
    var DATE_PATTERN = "yyyy-MM-dd";
    var DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    var LOCAL_TIMEZONE = Intl.DateTimeFormat().resolvedOptions().timeZone || "UTC";
    var CODE_LIST_FIELD_SUFFIX = "_code";

    /**
     * Create the URL for REST APIs with relative path
     *
     * @param {string} path Relative path
     * @returns {string} URL
     */
    var createUrl = function (path) {
      var restUri = ServiceUtils.getDataSource("restService").uri;
      return ServiceUtils.getUrl(restUri.concat(path));
    };

    return ManagedObject.extend("com.sap.gtt.app.sample.sst.controller.ReportEventsDialog", {

      state: {
        eventType: "",
        hasRefFields: false,
        isUserDefinedFieldsLoading: true,
        isEventReporting: false,
        userDefinedFieldBindingPaths: [],
        timeZoneOffsetMap: {},

        isPlannedEventValid: true,
        isUnplannedEventValid: true,
        isActualBusinessTimeValid: true,
        isActualBusinessTimeZoneValid: true,
        isReportedByValid: true,
      },

      /**
       * Create a new dialog instance
       *
       * @param {sap.ui.core.mvc.View} view Dependent view
       * @param {sap.ui.core.Component} [component] Component
       */
      constructor: function (view, component) {
        this._id = view.createId("reportEventsDialog");
        this._view = view;
        this._component = component;
        this._delegateComponentMethods();
      },

      /**
       * Init & Open this dialog
       *
       * @param {{
       *   eventStatue: "Planned"|"Unplanned",
       *   altKey: string,
       *   plannedEvents: object[],
       *   unplannedEvents?: object[],
       *   refPlannedEvents?: object[],
       *   timeZones?: object[],
       *   onReportSuccess?: Function,
       *   onReportFailed?: Function,
       * }} props Props
       */
      open: function (props) {
        this.props = props;

        if (!this._dialog) {
          Fragment.load({
            id: this._id,
            name: "com.sap.gtt.app.sample.sst.view.fragments.ReportEventsDialog",
            controller: this,
          }).then(function (dialog) {
            this._dialog = dialog;
            this._view.addDependent(this._dialog);
            this._dialog.setModel(new JSONModel(props).setDefaultBindingMode(BindingMode.OneWay), "props");
            this._dialog.setModel(new JSONModel(this.state, true), "state");

            // Additional models
            this._dialog.setModel(new JSONModel(), FORM_MODEL_NAME);

            this._dialog.open();
          }.bind(this));
        } else {
          this._dialog.getModel("props").setData(props);
          this._dialog.open();
        }
      },

      onAfterOpen: function () {
        // Setup timezone offset map (timeZoneCode -> offset)
        if (this.props.timeZones) {
          this.props.timeZones.forEach(function (timeZone) {
            this.state.timeZoneOffsetMap[timeZone.timeZoneCode] = timeZone.offset;
          }.bind(this));
        }

        // Set some default values for form
        var formModel = this._dialog.getModel(FORM_MODEL_NAME);

        // Format the current date as an ISO string with local time zone offset
        var date = new Date();
        /**
         * @example "2020-12-02T08:00:00.000+08:00"
         */
        var currentTimestamp = new Date(date.getTime() - (date.getTimezoneOffset() * 60000)).toISOString()
          .replace("Z", this.state.timeZoneOffsetMap[LOCAL_TIMEZONE] || "Z");

        formModel.setProperty("/actualBusinessTimestamp", currentTimestamp);
        formModel.setProperty("/actualBusinessTimeZone", LOCAL_TIMEZONE);

        // Enhance time zone filter function
        this.byId("actualBusinessTimeZone").setFilterFunction(function (term, item) {
          // A case-insensitive 'string contains' filter
          var re = new RegExp(term.replace(/\+/, "\\+"), "i");
          return re.test(item.getText()) || re.test(item.getAdditionalText());
        });

        // Reported by logon user by default
        if (sap.ushell) {
          sap.ushell.Container.getServiceAsync("UserInfo").then(function (userInfo) {
            formModel.setProperty("/reportedBy", userInfo.getUser().getEmail());
          });
        }
      },

      onAfterClose: function () {
        this.reset();
      },

      reset: function () {
        // Clear form data
        this._dialog.getModel(FORM_MODEL_NAME).setData({});
        this.byId("plannedEvent").setSelectedKey();
        this.byId("unplannedEvent").setSelectedKey();
        this.byId("refPlannedEvent").setSelectedKey();

        // Reset state
        Object.assign(this.state, {
          eventType: "",
          hasRefFields: false,
          isUserDefinedFieldsLoading: true,
          isEventReporting: false,
          userDefinedFieldBindingPaths: [],
          isPlannedEventValid: true,
          isUnplannedEventValid: true,
          isActualBusinessTimeValid: true,
          isActualBusinessTimeZoneValid: true,
          isReportedByValid: true,
        });

        this.removeAllUserDefinedFields();
      },

      close: function () {
        this._dialog.close();
      },

      exit: function () {
        delete this._view;
      },

      /**
       * Get inner element of this dialog by its ID
       *
       * @param {string} id ID
       * @return {sap.ui.core.Element} Element by its ID or undefined
       */
      byId: function (id) {
        return Fragment.byId(this._id, id);
      },

      /**
       * Create an ID based on this fragment
       *
       * @param {string} id ID
       * @return {string} New ID based on this fragment
       */
      createId: function (id) {
        return Fragment.createId(this._id, id);
      },

      /**
       * Handle planned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected planned event item
       */
      handlePlannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();

        // Bind selected planned event's info to form model
        var formModel = this._dialog.getModel(FORM_MODEL_NAME);
        formModel.setData({
          locationAltKey: selectedEvent.locationAltKey,
          eventMatchKey: selectedEvent.eventMatchKey,
        }, true);

        // Update eventType, quit if equal to current eventType
        var eventType = selectedEvent.eventType;
        if (eventType === this.state.eventType) {
          return;
        }

        this.state.eventType = eventType;
        this.state.isPlannedEventValid = true;

        this.updateUserDefinedFields(eventType);
      },

      /**
       * Handle unplanned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected unplanned event item with eventType key
       */
      handleUnplannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();
        var eventType = selectedEvent.eventType.target.split(".").pop();

        this.state.eventType = eventType;
        this.state.isUnplannedEventValid = true;

        this.updateUserDefinedFields(eventType);
      },

      /**
       * Handle reference planned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected reference planned event item
       */
      handleRefPlannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();

        // Bind selected reference planned event's info to form model
        var formModel = this._dialog.getModel(FORM_MODEL_NAME);
        formModel.setData({
          refPlannedEventType: selectedEvent.eventTypeFullName,
          refPlannedEventMatchKey: selectedEvent.eventMatchKey,
          refPlannedEventLocationAltKey: selectedEvent.locationAltKey,
        }, true);
      },

      /**
       * Handle actual business time change
       *
       * @param {boolean} valid Is actual business time valid or not
       */
      handleActualBusinessTimeChange: function (valid) {
        this.state.isActualBusinessTimeValid = valid;
      },

      /**
       * Handle actual business time zone change
       *
       * @param {string} value Time zone code
       * @param {boolean} itemPressed Is time zone selected
       */
      handleActualBusinessTimeZoneChange: function (value, itemPressed) {
        this.state.isActualBusinessTimeZoneValid = (
          itemPressed ||
          !value ||
          this.state.timeZoneOffsetMap[value]
        );
      },

      /**
       * Handle Reported By change
       *
       * @param {string} value  Value of Reported By
       */
      handleReportedByChange: function (value) {
        this.state.isReportedByValid = (value !== "");
      },

      /**
       * Update user defined fields
       *
       * @param {string} eventType Event type
       */
      updateUserDefinedFields: function (eventType) {
        this.state.isUserDefinedFieldsLoading = true;

        var url = createUrl("/events/" + eventType + "/fields");
        RestClient.get(url).then(function (response) {
          var userDefinedFields = response.elements.filter(function (field) {
            return !field.isFromCoreModel;
          });
          this.removeAllUserDefinedFields();
          this.insertUserDefinedFields(userDefinedFields);
          this.state.isUserDefinedFieldsLoading = false;

          switch (response.parent.target) {
            case "CoreModel.GTTOnTimeEvent":
            case "CoreModel.GTTDelayedEvent":
              this.state.hasRefFields = true;
              break;

            case "CoreModel.Event":
              this.state.hasRefFields = false;

              // Clear up Referenced Planned Event
              var formModel = this._dialog.getModel(FORM_MODEL_NAME);
              formModel.setProperty("/refPlannedEventType", undefined);
              formModel.setProperty("/refPlannedEventMatchKey", undefined);
              formModel.setProperty("/refPlannedEventLocationAltKey", undefined);

              this.byId("refPlannedEvent").setSelectedKey();
              break;
          }
        }.bind(this), function (error) {
          this.handleHttpError(error);
          this.state.isUserDefinedFieldsLoading = false;
        }.bind(this));
      },

      /**
       * Insert user defined fields into the form
       *
       * @param {object[]} fields User defined fields
       */
      insertUserDefinedFields: function (fields) {
        fields.forEach(function (field) {
          /** @type {sap.ui.core.Control} */
          var input;
          var id = this.createId(field.name);
          /** @type {sap.ui.model.SimpleType} */
          var formatType;
          var bindingPath = FORM_MODEL_NAME + ">/" + field.name;

          // Generate input control
          switch (field.type) {
            case CustomField.Type.Integer:
              input = new Input(id, {
                type: sap.m.InputType.Number,
              });
              formatType = new sap.ui.model.type.Integer({
                emptyString: null,
              });
              break;

            case CustomField.Type.Decimal:
              input = new Input(id, {
                type: sap.m.InputType.Number,
              });
              formatType = new sap.ui.model.type.Float({
                minFractionDigits: field.precision - field.scale,
                maxFractionDigits: field.precision + field.scale,
                emptyString: null,
              });
              break;

            case CustomField.Type.Boolean:
              input = new Select(id, {
                forceSelection: false,
              });
              [
                { key: null, i18nKey: "noneSelected" },
                { key: true, i18nKey: "yes" },
                { key: false, i18nKey: "no" },
              ].forEach(function (selection) {
                input.addItem(new Item({
                  key: selection.key,
                  text: this.getText(selection.i18nKey),
                }));
              }.bind(this));
              break;

            case CustomField.Type.CodeList:
              input = new Select(id, {
                forceSelection: false,
                enabled: false,
                busy: true,
              });
              input.addItem(new Item({
                key: null,
                text: this.getText("noneSelected"),
              }));

              // Fetch the code list
              var url = createUrl("/events/codeLists/" + field.target.split(".").pop());
              RestClient.get(url).then(function (response) {
                // Insert each code list item as selection
                response.forEach(function (value) {
                  input.addItem(new Item({
                    key: value.code,
                    text: value.localized.name ? value.localized.name : value.name,
                  }));
                });
                input.setEnabled(true);
                input.setBusy(false);
              }, function (error) {
                this.handleHttpError(error);
                input.setBusy(false);
              }.bind(this));

              bindingPath += CODE_LIST_FIELD_SUFFIX;
              break;

            case CustomField.Type.Date:
              input = new DatePicker(id, {
                valueFormat: DATE_PATTERN,
                valueStateText: this.getText("wrongFormatWarningMsg"),
                change: function (event) {
                  var source = event.getSource();
                  var valid = event.getParameters().valid;
                  source.setValueState(valid ? ValueState.None : ValueState.Error);
                },
              });
              break;

            case CustomField.Type.Timestamp:
              input = new DateTimePicker(id, {
                valueFormat: DATE_TIME_PATTERN,
                valueStateText: this.getText("wrongFormatWarningMsg"),
                change: function (event) {
                  var source = event.getSource();
                  var valid = event.getParameters().valid;
                  source.setValueState(valid ? ValueState.None : ValueState.Error);
                },
              });
              break;

            case CustomField.Type.Association:
              return;

            // TODO: Not supported
            case CustomField.Type.Composition:
              return;

            case CustomField.Type.String:
              input = new Input(id, {
                maxLength: field.length,
              });
              break;

            case CustomField.Type.UUID:
            default:
              input = new Input(id);
              break;
          }

          // Bind input value to form model
          switch (input.getMetadata().getName()) {
            case "sap.m.Input":
            case "sap.m.DatePicker":
            case "sap.m.DateTimePicker":
              input.bindValue({
                path: bindingPath,
                type: formatType,
              });
              break;
            case "sap.m.Select":
              input.bindProperty("selectedKey", bindingPath);
              break;
            default:
              return;
          }

          // Save its bindingPath
          this.state.userDefinedFieldBindingPaths.push(bindingPath);

          // Init form element
          var formElement = new FormElement();
          formElement.setLabel(this.getText(field.i18nKey, null, "@i18n") || field.name);
          formElement.addField(input);

          // Insert form element into form container
          this.getUserDefinedFieldsFormContainer().addFormElement(formElement);
        }.bind(this));
      },

      /**
       * Remove all user defined fields from form
       */
      removeAllUserDefinedFields: function () {
        var formModel = this._dialog.getModel(FORM_MODEL_NAME);

        // Clear user defined fields' data
        this.state.userDefinedFieldBindingPaths.forEach(function (bindingPath) {
          var relativePath = bindingPath.split(">").pop();
          formModel.setProperty(relativePath, undefined);
        });

        // Reset binding paths
        this.state.userDefinedFieldBindingPaths = [];

        this.getUserDefinedFieldsFormContainer().destroyFormElements();
      },

      /**
       * Get user defined fields form container
       *
       * @returns {sap.ui.layout.form.FormContainer} User defined fields form container
       */
      getUserDefinedFieldsFormContainer: function () {
        return this.byId("userDefinedFieldsFormContainer");
      },

      reportEvent: function () {
        // Read form data
        var formData = this._dialog.getModel(FORM_MODEL_NAME).getData();

        // Validate form data, quit if is invalid
        if (!this.state.eventType) {
          this.state.isPlannedEventValid = false;
          this.state.isUnplannedEventValid = false;
          return;
        }
        if (
          !formData.actualBusinessTimestamp ||
          !this.state.isActualBusinessTimeValid
        ) {
          this.state.isActualBusinessTimeValid = false;
          return;
        }
        if (!this.state.isActualBusinessTimeZoneValid) {
          return;
        }
        if (
          !formData.reportedBy ||
          !this.state.isReportedByValid
        ) {
          this.state.isReportedByValid = false;
          return;
        }

        // Build up payload
        var payload = Object.assign({}, formData, {
          altKey: this.props.altKey,
          actualBusinessTimestamp: formData.actualBusinessTimestamp.replace(/((\+|-)\d{2}:\d{2}|Z)$/g, "") + (
            formData.actualBusinessTimeZone
              ? this.state.timeZoneOffsetMap[formData.actualBusinessTimeZone]
              : "Z"
          ),
        });

        // Clean up payload (Delete `""` and `null`)
        Object.keys(payload).forEach(function (key) {
          if (payload[key] === "" || payload[key] === null) {
            delete payload[key];
          }
        });

        // Report new event
        this.state.isEventReporting = true;

        var url = createUrl("/events/" + this.state.eventType);
        RestClient.post(url, payload).then(function () {
          this.close();

          if (this.props.onReportSuccess) {
            this.props.onReportSuccess();
          }
        }.bind(this), function (error) {
          this.handleHttpError(error, payload);
          this.state.isEventReporting = false;

          if (this.props.onReportFailed) {
            this.props.onReportFailed();
          }
        }.bind(this));
      },

      /**
       * Handle HTTP error
       *
       * @param {object} error HTTP error
       * @param {*} details Error details
       */
      handleHttpError: function (error, details) {
        var response = error.response;
        MessageBox.error(response.data, {
          title: response.status + " " + response.statusText,
          details: details,
        });
      },

      // ======================================================================
      // External Dependencies
      // ======================================================================

      formatter: formatter,

      _delegateComponentMethods: function () {
        var methodNames = ["getText", "getPropertyLabelText"];

        methodNames.forEach(function (methodName) {
          this[methodName] = function () {
            var component = this._component;
            return component[methodName].apply(component, arguments);
          };
        }.bind(this));
      },
    });
  }
);
