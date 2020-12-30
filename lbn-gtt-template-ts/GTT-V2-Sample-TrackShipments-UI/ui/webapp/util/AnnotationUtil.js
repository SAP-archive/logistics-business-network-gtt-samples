sap.ui.define(
  [
    "sap/base/strings/formatMessage",
  ],
  function (
    formatMessage
  ) {
    "use strict";

    var metaModel;
    var annotations;
    var edmxNamespace;

    /**
     * Utility class for processing OData Annotations
     *
     */
    return {

      init: function (options) {
        metaModel = options.metaModel;
        annotations = options.annotations;
        this.setEdmxNamespace();
      },

      setEdmxNamespace: function () {
        var entityContainer = metaModel.getODataEntityContainer();
        edmxNamespace = entityContainer.namespace;
      },


      // ============================================================
      // Property
      // ============================================================

      /**
       * Get property label
       * @param {string} propertyName The property name of an entity type
       * @param {string} entitySet The entity set (in gtt v2, it is the same as entity type)
       * @return {string} The property label which could be a string or binding info like '{@i18n>propertyName}'
       */
      getPropertyLabel: function (propertyName, entitySet) {
        var fullName = formatMessage("{0}.{1}", edmxNamespace, entitySet);
        var propertyAnnotation = annotations.propertyAnnotations[fullName][propertyName];

        if (!propertyAnnotation) {
          return propertyName;
        }

        // if no label is set for property use property name as fallback
        return propertyAnnotation["com.sap.vocabularies.Common.v1.Label"]
          ? propertyAnnotation["com.sap.vocabularies.Common.v1.Label"].String
          : propertyName;
      },
    };
  }
);
