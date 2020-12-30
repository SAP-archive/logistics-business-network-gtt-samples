sap.ui.define([
  "sap/ui/core/XMLComposite",
], function (XMLComposite) {
  "use strict";

  return XMLComposite.extend("com.sap.gtt.app.sample.pof.control.popover.PopoverList", jQuery.extend({}, {
    metadata: {
      properties: {
        title         : {type: "string", defaultValue: ""},
        placement     : {type: "sap.m.PlacementType"},
        contentWidth  : {type: "sap.ui.core.CSSSize", defaultValue: "22rem"},
        contentHeight : {type: "sap.ui.core.CSSSize", defaultValue: "12rem"},
      },
      aggregations: {
        popoverContent: {
          type: "sap.ui.core.Control",
          multiple: false,
          forwarding: {
            idSuffix: "--innerVBox",
            aggregation: "items",
          },
        },
        items: {
          type: "sap.m.ListItemBase",
          multiple: true,
          forwarding: {
            idSuffix: "--innerList",
            aggregation: "items",
          },
        },
      },
      defaultAggregation  : "content",
      events: {
        open: {},
      },
    },

    getPopover: function () {
      return this.byId("popover");
    },

    open: function (oSourceControl) {
      this.getPopover().openBy(oSourceControl);
    },
  }));
});
