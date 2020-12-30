sap.ui.define(
  [
  ],
  function (
  ) {
    "use strict";

    var metaModel;

    /**
     * Utility class for processing OData Annotations
     *
     */
    return {

      init: function (options) {
        metaModel = options.metaModel;
      },

      // ============================================================
      // Entity
      // ============================================================

      getEntityTypeByEntitySet: function (entitySetName) {
        var entitySet = metaModel.getODataEntitySet(entitySetName);
        return metaModel.getODataEntityType(entitySet.entityType);
      },

      // ============================================================
      // Property
      // ============================================================

      getProperty: function (propertyName, entityType) {
        return metaModel.getODataProperty(entityType, propertyName, false);
      },

      getPropertyLabel: function (propertyName, entitySet) {
        var entityType = this.getEntityTypeByEntitySet(entitySet);
        var property = this.getProperty(propertyName, entityType);

        if (!property) {
          return propertyName;
        }

        // if no label is set for property use property name as fallback
        return property["com.sap.vocabularies.Common.v1.Label"]
          ? property["com.sap.vocabularies.Common.v1.Label"].String
          : property.name;
      },
    };
  }
);
