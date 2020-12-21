sap.ui.define([], function () {
  "use strict";

  var CustomField = {};

  /**
   * Enum type for the custom field
   *
   * @readonly
   * @enum {string}
   */
  CustomField.Type = {
    UUID: "uuid",
    String: "string",
    Boolean: "boolean",
    Integer: "integer",
    Decimal: "decimal",
    Date: "date",
    Timestamp: "timestamp",
    Association: "association",
    Composition: "composition",
    CodeList: "codelist",
  };

  return CustomField;
});
