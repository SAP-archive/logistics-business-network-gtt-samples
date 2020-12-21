sap.ui.define([
  "sap/ui/core/Control",
], function (Control) {
  "use strict";

  return Control.extend("com.sap.gtt.app.sample.sst.controls.CurrentLocation", {

    library : "com.sap.gtt.app.sample.sst.controls",
    metadata: {
      properties: {
        color: {
          type: "string",
          group: "Misc",
          defaultValue: "rgb(92, 186, 230)",
        },
      },
    },

    init: function () {
    },

    renderer: function (renderManager, control) {
      // convenience variable
      var rm = renderManager;
      rm.openStart("div", control); // it will render the control data like id and custom data
      rm.class("sstMapCurrentLocation");
      rm.attr("title", control.getTooltip());
      rm.openEnd();

      rm.write("<svg");
      rm.attr("width", "18px");
      rm.attr("height", "18px");
      rm.write(">");

      rm.write("<circle");
      rm.attr("cx", 9);
      rm.attr("cy", 9);
      rm.attr("r", 9);
      rm.attr("stoke", "black");
      rm.attr("stoke-width", 2);
      rm.attr("fill", control.getColor());
      rm.write("/>");
      rm.write("</svg>");

      rm.close("div");
    },
  });
});
