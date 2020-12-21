sap.ui.define([
  "sap/ui/core/XMLComposite",
  "com/sap/gtt/app/sample/pof/controller/mixins/TextHelper.mixin",
], function (XMLComposite, TextHelper) {
  "use strict";

  return XMLComposite.extend("com.sap.gtt.app.sample.pof.control.link.LinkPopover", jQuery.extend({}, {
    metadata: {
      properties: {
        title         : {type: "string", defaultValue: TextHelper.getText("noTitleLabel")},
        text          : {type: "string", defaultValue: ""},
        linkText      : {type: "string"},
        relatedObject : {type: "string"},
        placement     : {type: "sap.m.PlacementType"},
        contentWidth  : {type: "sap.ui.core.CSSSize", defaultValue: "22rem"},
        contentHeight : {type: "sap.ui.core.CSSSize", defaultValue: "12rem"},
      },
      aggregations: {
        content: {
          type: "sap.ui.core.Control",
          multiple: false,
          forwarding: {
            idSuffix: "--reportingPopover",
            aggregation: "popoverContent",
          },
        },
        popoverItems: {
          type: "sap.m.ListItemBase",
          multiple: true,
          forwarding: {
            idSuffix: "--reportingPopover",
            aggregation: "items",
          },
        },
      },
      defaultAggregation  : "popoverItems",
      events: {
        press: {},
      },
    },

    setLinkText: function (sLinkText) {
      var oContext = this.getBindingContext();
      if(!oContext) {
        this.setProperty("linkText", "");
        return;
      }
      if (oContext.getObject(this.getProperty("relatedObject")) !== undefined) {
        if (!oContext.getObject(this.getProperty("relatedObject"))) {
          this.setProperty("linkText", "");
        } else {
          this.setProperty("linkText", sLinkText);
        }
      } else {
        this.setProperty("linkText", sLinkText);
      }
    },

    getListPopover: function () {
      return this.byId("reportingPopover");
    },

    onLinkPress: function (oEvent) {
      oEvent.preventDefault();
      if(!!this.getContent() && !this.getContent().getBindingContext()) {
        this.getContent().setModel(this.getModel("i18n"), "i18n");
        this.getContent().setModel(this.getModel());
        this.getContent().setBindingContext(this.getBindingContext());
      }
      this.getListPopover().open(oEvent.getSource());
      this.firePress(oEvent);
    },
  }));
});
