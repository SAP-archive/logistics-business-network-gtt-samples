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
    "sap/base/util/deepClone",
    "sap/base/util/isEmptyObject",
    "../model/formatter",
    "../util/registerState",
    "../util/i18n",
    "../api/modelApi",
    "../api/eventApi",
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
    deepClone,
    isEmptyObject,
    formatter,
    registerState,
    i18n,
    modelApi,
    eventApi,
    CustomField
  ) {
    "use strict";

    var FORM_MODEL_NAME = "eventDetail";
    var DATE_PATTERN = "yyyy-MM-dd";
    var DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    var LOCAL_TIMEZONE = Intl.DateTimeFormat().resolvedOptions().timeZone || "UTC";
    var CODE_LIST_FIELD_SUFFIX = "_code";

    return ManagedObject.extend("com.sap.gtt.app.sample.sst.controller.ReportEventsDialog", {

      state: {
        eventType: "",
        hasRefFields: false,
        isUserDefinedFieldsLoading: true,
        isEventReporting: false,
        userDefinedFieldBindingPaths: [],
        timeZoneOffsetMap: {},
        deliveryItems: [],

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
       */
      constructor: function (view) {
        this._id = view.createId("reportEventsDialog");
        this._view = view;
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
       *   disableDeliveryItemsReporting?: boolean,
       *   onReportSuccess?: () => void,
       *   onReportFailed?: () => void,
       *   onClose?: () => void,
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

            registerState(this.state, this);
            this._dialog.setModel(new JSONModel(this.state, true), "state");

            // Additional models
            this._dialog.setModel(new JSONModel(), FORM_MODEL_NAME);
            this._formModel = this._dialog.getModel(FORM_MODEL_NAME);

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

        // Format the current date as an ISO string with local time zone offset
        var date = new Date();
        /**
         * @example "2020-12-02T08:00:00.000+08:00"
         */
        var currentTimestamp = new Date(date.getTime() - (date.getTimezoneOffset() * 60000)).toISOString()
          .replace("Z", this.state.timeZoneOffsetMap[LOCAL_TIMEZONE] || "Z");

        // Set default values
        this.setFieldsValue({
          actualBusinessTimestamp: currentTimestamp,
          actualBusinessTimeZone: LOCAL_TIMEZONE,
        });

        // Enhance time zone filter function
        this.byId("actualBusinessTimeZone").setFilterFunction(function (term, item) {
          // A case-insensitive 'string contains' filter
          var re = new RegExp(term.replace(/\+/, "\\+"), "i");
          return re.test(item.getText()) || re.test(item.getAdditionalText());
        });

        // Reported by logon user by default
        if (sap.ushell && sap.ushell.Container) {
          sap.ushell.Container.getServiceAsync("UserInfo").then(function (userInfo) {
            this.setFieldValue("reportedBy", userInfo.getUser().getEmail());
          }.bind(this));
        }
      },

      onAfterClose: function () {
        this.reset();
        if (this.props.onClose) {
          this.props.onClose();
        }
      },

      reset: function () {
        // Clear form data
        this._formModel.setData({});
        this.byId("plannedEvent").setSelectedKey();
        this.byId("unplannedEvent").setSelectedKey();
        this.byId("refPlannedEvent").setSelectedKey();

        this.resetState();

        this.removeAllUserDefinedFields();
      },

      close: function () {
        this._dialog.close();
      },

      destory: function () {
        this._dialog.destroy();
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
        var element = Fragment.byId(this._id, id);

        if (!element) {
          throw new Error(
            "Element with ID '" + id + "' does not exist in the '" + this._id + "' Fragment."
          );
        }

        return element;
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
       * Update user defined fields when `eventType` changes
       *
       * @param {object} changes Value changes of `eventType`
       */
      onEventTypeChange: function (changes) {
        this.updateUserDefinedFields(changes.current);
      },

      /**
       * Handle planned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected planned event item
       */
      handlePlannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();

        // Bind selected planned event's info to form model
        this.setFieldsValue({
          locationAltKey: selectedEvent.locationAltKey,
          eventMatchKey: selectedEvent.eventMatchKey,
        });

        this.setEventType(selectedEvent.eventType);
        this.setIsPlannedEventValid(true);

        this.updateDeliveryItemsTable((
          !this.props.disableDeliveryItemsReporting &&
          selectedEvent.eventType === "POD" &&
          Array.isArray(selectedEvent.deliveryItems)
        ) ? selectedEvent.deliveryItems : []);
      },

      /**
       * Handle unplanned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected unplanned event item with eventType key
       */
      handleUnplannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();
        var eventType = selectedEvent.eventType.target.split(".").pop();

        this.setEventType(eventType);
        this.setIsUnplannedEventValid(true);
      },

      /**
       * Handle reference planned event select
       *
       * @param {sap.ui.core.Item} selectedItem Selected reference planned event item
       */
      handleRefPlannedEventSelect: function (selectedItem) {
        var selectedEvent = selectedItem.getBindingContext("props").getObject();

        // Bind selected reference planned event's info to form model
        this.setFieldsValue({
          refPlannedEventType: selectedEvent.eventTypeFullName,
          refPlannedEventMatchKey: selectedEvent.eventMatchKey,
          refPlannedEventLocationAltKey: selectedEvent.locationAltKey,
        });
      },

      /**
       * Handle actual business time change
       *
       * @param {boolean} valid Is actual business time valid or not
       */
      handleActualBusinessTimeChange: function (valid) {
        this.setIsActualBusinessTimeValid(valid);
      },

      /**
       * Handle actual business time zone change
       *
       * @param {string} value Time zone code
       * @param {boolean} itemPressed Is time zone selected
       */
      handleActualBusinessTimeZoneChange: function (value, itemPressed) {
        this.setIsActualBusinessTimeZoneValid(
          itemPressed ||
          !value ||
          !!this.state.timeZoneOffsetMap[value]
        );
      },

      /**
       * Handle Reported By change
       *
       * @param {string} value  Value of Reported By
       */
      handleReportedByChange: function (value) {
        this.setIsReportedByValid(value !== "");
      },

      /**
       * Update user defined fields
       *
       * @param {string} eventType Event type
       */
      updateUserDefinedFields: function (eventType) {
        this.setIsUserDefinedFieldsLoading(true);

        modelApi.getFieldsByEventType("Shipment", eventType).then(function (response) {
          this.removeAllUserDefinedFields();

          response.elements.filter(function (fieldInfo) {
            return !fieldInfo.isFromCoreModel;
          }).forEach(function (fieldInfo) {
            var bindingPath = FORM_MODEL_NAME + ">/" + fieldInfo.name;
            if (fieldInfo.type === CustomField.Type.CodeList) {
              bindingPath += CODE_LIST_FIELD_SUFFIX;
            }

            var field = this.generateUserDefinedField({
              id: this.createId(fieldInfo.name),
              fieldInfo: fieldInfo,
              bindingPath: bindingPath,
            });

            if (field) {
              this.insertUserDefinedField({
                label: this.getLocalizedLabel(fieldInfo),
                field: field,
              });
              // Save its bindingPath
              this.state.userDefinedFieldBindingPaths.push(bindingPath);
            }
          }, this);

          this.setIsUserDefinedFieldsLoading(false);

          switch (response.parent.target) {
            case "CoreModel.GTTOnTimeEvent":
            case "CoreModel.GTTDelayedEvent":
              this.setHasRefFields(true);
              break;

            case "CoreModel.Event":
              this.setHasRefFields(false);

              // Clear up Referenced Planned Event
              this.setFieldsValue({
                refPlannedEventType: undefined,
                refPlannedEventMatchKey: undefined,
                refPlannedEventLocationAltKey: undefined,
              });

              this.byId("refPlannedEvent").setSelectedKey();
              break;
          }
        }.bind(this), function (error) {
          this.handleHttpError(error);
          this.setIsUserDefinedFieldsLoading(false);
        }.bind(this));
      },

      /**
       * Generate user defined field
       *
       * @param {object} options Generation options
       * @param {EventTypeFieldInfo} options.fieldInfo Field's info
       * @param {string} options.bindingPath Binding path
       * @param {string} [options.id=""] Field's ID
       * @param {boolean} [options.shouldRenderCompositionTable=true] Should render composition table or not
       *
       * @returns {sap.ui.core.Control|undefined} The generated field
       */
      generateUserDefinedField: function (options) {
        var defaultOptions = {
          id: "",
          shouldRenderCompositionTable: true,
        };
        var opts = Object.assign({}, defaultOptions, options);

        var fieldInfo = opts.fieldInfo;
        var bindingPath = opts.bindingPath;
        var id = opts.id;
        var shouldRenderCompositionTable = opts.shouldRenderCompositionTable;

        /** @type {sap.ui.core.Control} */
        var field;
        /** @type {sap.ui.model.SimpleType} */
        var valueType;

        // Generate field control
        switch (fieldInfo.type) {
          case CustomField.Type.Integer:
            field = new Input(id, {
              type: sap.m.InputType.Number,
            });
            valueType = new sap.ui.model.type.Integer({
              emptyString: null,
            });
            break;

          case CustomField.Type.Decimal:
            field = new Input(id, {
              type: sap.m.InputType.Number,
            });
            valueType = new sap.ui.model.type.Float({
              minFractionDigits: fieldInfo.precision - fieldInfo.scale,
              maxFractionDigits: fieldInfo.precision + fieldInfo.scale,
              emptyString: null,
            });
            break;

          case CustomField.Type.Boolean:
            field = new Select(id, {
              forceSelection: false,
            });
            [
              { key: null, i18nKey: "noneSelected" },
              { key: true, i18nKey: "yes" },
              { key: false, i18nKey: "no" },
            ].forEach(function (selection) {
              field.addItem(new Item({
                key: selection.key,
                text: i18n(selection.i18nKey),
              }));
            }.bind(this));
            break;

          case CustomField.Type.CodeList:
            field = new Select(id, {
              forceSelection: false,
              enabled: false,
              busy: true,
            });
            field.addItem(new Item({
              key: null,
              text: i18n("noneSelected"),
            }));

            // Fetch the code list
            eventApi.getCodeListByName(
              fieldInfo.target.split(".").pop()
            ).then(function (response) {
              // Insert each code list item as selection
              response.forEach(function (value) {
                field.addItem(new Item({
                  key: value.code,
                  text: value.localized.name ? value.localized.name : value.name,
                }));
              });
              field.setEnabled(true);
              field.setBusy(false);
            }, function (error) {
              this.handleHttpError(error);
              field.setBusy(false);
            }.bind(this));
            break;

          case CustomField.Type.Date:
            field = new DatePicker(id, {
              valueFormat: DATE_PATTERN,
              valueStateText: i18n("wrongFormatWarningMsg"),
              change: function (event) {
                var source = event.getSource();
                var valid = event.getParameters().valid;
                source.setValueState(valid ? ValueState.None : ValueState.Error);
              },
            });
            break;

          case CustomField.Type.Timestamp:
            field = new DateTimePicker(id, {
              valueFormat: DATE_TIME_PATTERN,
              valueStateText: i18n("wrongFormatWarningMsg"),
              change: function (event) {
                var source = event.getSource();
                var valid = event.getParameters().valid;
                source.setValueState(valid ? ValueState.None : ValueState.Error);
              },
            });
            break;

          case CustomField.Type.Association:
            // Skip
            return undefined;

          case CustomField.Type.Composition:
            // Need core-engine support
            // if (shouldRenderCompositionTable) {
            //   this.insertCompositionTable(fieldInfo, bindingPath);
            // }
            return undefined;

          case CustomField.Type.String:
            field = new Input(id, {
              maxLength: fieldInfo.length,
            });
            break;

          case CustomField.Type.UUID:
          default:
            field = new Input(id);
            break;
        }

        // Bind field value to the form model
        switch (field.getMetadata().getName()) {
          case "sap.m.Input":
          case "sap.m.DatePicker":
          case "sap.m.DateTimePicker":
            field.bindValue({
              path: bindingPath,
              type: valueType,
            });
            break;
          case "sap.m.Select":
            field.bindProperty("selectedKey", bindingPath);
            break;
          default:
            return undefined;
        }

        return field;
      },

      /**
       * Insert user defined field into the form
       *
       * @param {object} options Field options
       * @param {string} options.label Field label
       * @param {sap.ui.core.Control} options.field Field control
       */
      insertUserDefinedField: function (options) {
        /** @type {sap.ui.layout.form.FormElement} */
        var formElement = new FormElement();
        formElement.setLabel(options.label);
        formElement.addField(options.field);

        this.getUserDefinedFieldsFormContainer().addFormElement(formElement);
      },

      insertCompositionTable: function (fieldInfo, bindingPath) {
        var compositionTable = new sap.ui.table.Table("", {
          enableColumnReordering: false,
          visibleRowCountMode: "Interactive",
          visibleRowCount: 3,
          minAutoRowCount: 1,
          alternateRowColors: true,
          selectionMode: "None",
          rowActionCount: 1,
          rowActionTemplate: new sap.ui.table.RowAction("", {
            items: new sap.ui.table.RowActionItem("", {
              icon: "sap-icon://decline",
              press: function (event) {
                var rowIndex = event.getParameters().row.getIndex();
                var compositionItems = this.getFieldValue(fieldInfo.name);

                compositionItems.splice(rowIndex, 1);
                this.setFieldValue(fieldInfo.name, compositionItems);
              }.bind(this),
            }),
          }),
        }).bindRows(bindingPath);

        var compositionPanel = new sap.m.Panel("", {
          headerToolbar: new sap.m.OverflowToolbar("", {
            content: [
              new sap.m.Text({
                text: this.getLocalizedLabel(fieldInfo),
              }).addStyleClass("sapMPanelHdr"),
              new sap.m.ToolbarSpacer(),
              new sap.m.Button({
                text: i18n("add"),
                press: function () {
                  this.setFieldValue(
                    fieldInfo.name,
                    this.getFieldValue(fieldInfo.name).concat([{}])
                  );
                }.bind(this),
              }),
            ],
            active: true,
            press: function () {
              compositionPanel.setExpanded(!compositionPanel.getExpanded());
            },
          }),
          content: compositionTable,
          expandable: true,
          busy: true,
        });

        modelApi.getFieldsByEventType(
          "Shipment",
          fieldInfo.target.split(".").pop()
        ).then(function (response) {
          response.elements.forEach(function (childFieldInfo) {
            compositionTable.addColumn(new sap.ui.table.Column("", {
              label: new sap.m.Label({
                text: this.getLocalizedLabel(childFieldInfo),
              }),
              template: this.generateUserDefinedField({
                fieldInfo: childFieldInfo,
                bindingPath: FORM_MODEL_NAME + ">" + childFieldInfo.name,
                shouldRenderCompositionTable: false, // Nested composition is currently not supported
              }),
              minWidth: 240,
            }));
          }, this);
          compositionPanel.setBusy(false);
        }.bind(this), function (error) {
          this.handleHttpError(error);
          compositionPanel.setBusy(false);
        }.bind(this));

        this.getCompositionTableVBox().addItem(compositionPanel);
        this.setFieldValue(fieldInfo.name, [{}]);
      },

      /**
       * Remove all user defined fields from form
       */
      removeAllUserDefinedFields: function () {
        // Clear user defined fields' data
        this.state.userDefinedFieldBindingPaths.forEach(function (bindingPath) {
          this.setFieldValue(bindingPath.split(">/").pop(), undefined);
        }, this);

        // Reset binding paths
        this.setUserDefinedFieldBindingPaths([]);

        this.getUserDefinedFieldsFormContainer().destroyFormElements();
        this.getCompositionTableVBox().destroyItems();
      },

      /**
       * Update the delivery items table
       *
       * @param {object[]} deliveryItems Delivery items
       */
      updateDeliveryItemsTable: function (deliveryItems) {
        this.setDeliveryItems(deliveryItems.map(function (deliveryItem) {
          return Object.assign({}, deliveryItem, {
            quantity: deliveryItem.orderQuantity,
          });
        }));
      },

      /**
       * Get field value by name
       *
       * @param {string} name Field name (Add `"_code"` suffix if is a `CodeList` field)
       * @returns {any} Field value
       */
      getFieldValue: function (name) {
        return deepClone(
          this._formModel.getProperty("/" + name)
        );
      },

      /**
       * Set field value
       *
       * @param {string} name Field name (Add `"_code"` suffix if is a `CodeList` field)
       * @param {any} value Field value
       */
      setFieldValue: function (name, value) {
        this._formModel.setProperty("/" + name, value);
      },

      /**
       * Set values for multiple fields
       *
       * @param {object} values Values
       */
      setFieldsValue: function (values) {
        this._formModel.setData(values, /* bMerge= */ true);
      },

      /**
       * Get user defined fields form container
       *
       * @returns {sap.ui.layout.form.FormContainer} User defined fields form container
       */
      getUserDefinedFieldsFormContainer: function () {
        return this.byId("userDefinedFieldsFormContainer");
      },

      /**
       * Get composition table VBox
       *
       * @returns {sap.m.VBox} Composition table VBox
       */
      getCompositionTableVBox: function () {
        return this.byId("compositionTableVBox");
      },

      reportEvent: function () {
        // Read form data
        var formData = this._formModel.getData();

        // Validate form data, quit if is invalid
        if (!this.state.eventType) {
          this.setIsPlannedEventValid(false);
          this.setIsUnplannedEventValid(false);
          return;
        }
        if (
          !formData.actualBusinessTimestamp ||
          !this.state.isActualBusinessTimeValid
        ) {
          this.setIsActualBusinessTimeValid(false);
          return;
        }
        if (!this.state.isActualBusinessTimeZoneValid) {
          return;
        }
        if (
          !formData.reportedBy ||
          !this.state.isReportedByValid
        ) {
          this.setIsReportedByValid(false);
          return;
        }

        // Build up event payload
        var eventPayload = Object.assign({}, formData, {
          altKey: this.props.altKey,
          actualBusinessTimestamp: formData.actualBusinessTimestamp.replace(/((\+|-)\d{2}:\d{2}|Z)$/g, "") + (
            formData.actualBusinessTimeZone ? this.state.timeZoneOffsetMap[formData.actualBusinessTimeZone] : "Z"
          ),
        });

        // Clean up event payload (Delete `""` and `null`)
        Object.keys(eventPayload).forEach(function (key) {
          if (eventPayload[key] === "" || eventPayload[key] === null) {
            delete eventPayload[key];
          }

          // Remove empty composition items
          // if (Array.isArray(eventPayload[key])) {
          //   eventPayload[key] = eventPayload[key].filter(function (item) {
          //     return !isEmptyObject(item);
          //   });
          //   if (!eventPayload[key].length) {
          //     delete eventPayload[key];
          //   }
          // }
        });

        // Report new event & delivery items
        this.setIsEventReporting(true);

        var payloads = [eventPayload].concat(
          // Build up payload for each delivery item extends from `eventPayload`
          this.state.deliveryItems.filter(function (deliveryItem) {
            return deliveryItem.quantity > 0;
          }).map(function (deliveryItem) {
            return Object.assign({}, eventPayload, {
              altKey: deliveryItem.altKey,
              eventMatchKey: deliveryItem.eventMatchKey,
              quantity: deliveryItem.quantity,
              priority: eventPayload.priority ? (eventPayload.priority + 1) : 1,
            });
          })
        );

        Promise.allSettled(
          payloads.map(function (payload) {
            return eventApi.report(this.state.eventType, payload);
          }, this)
        ).then(function (
          /** @type {PromiseSettledResult<any>[]} */
          results
        ) {
          var failedIndex = results.findIndex(function (result) {
            return result.status === "rejected";
          });

          if (failedIndex === -1) {
            this.close();
          } else {
            this.handleHttpError(results[failedIndex].reason, payloads[failedIndex]);
            this.setIsEventReporting(false);
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

      /**
       * Get localized label of given field info
       *
       * @param {object} fieldInfo Field info
       * @returns {string} The localized label of given field info
       */
      getLocalizedLabel: function (fieldInfo) {
        return i18n(fieldInfo.i18nKey) || fieldInfo.name;
      },

      // ======================================================================
      // External Dependencies
      // ======================================================================

      formatter: formatter,
    });
  }
);
