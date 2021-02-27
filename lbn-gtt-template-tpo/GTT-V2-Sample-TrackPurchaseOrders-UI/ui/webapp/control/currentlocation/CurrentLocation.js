sap.ui.define([
  "sap/ui/core/Control",
  "com/sap/gtt/app/sample/pof/util/Constants",
], function (Control, Constants) {
  "use strict";

  return Control.extend("com.sap.gtt.app.sample.pof.control.currentlocation.CurrentLocation", {

    library : "com.sap.gtt.app.sample.pof.control",
    metadata: {
      properties: {
        color: {
          type: "string",
          group: "Misc",
          defaultValue: Constants.COLORS.CURRENT_LOCATION,
        },
      },
    },

    init: function () {
    },

    renderer: function (oRM, oControl) {
      // convenience variable
      oRM.openStart("div", oControl);
      oRM.class("currentLocationMap");
      oRM.attr("title", oControl.getTooltip());
      oRM.openEnd();

      oRM.write("<svg");
      oRM.attr("width", "18px");
      oRM.attr("height", "18px");
      oRM.write(">");

      oRM.write("<circle");
      oRM.attr("cx", 9);
      oRM.attr("cy", 9);
      oRM.attr("r", 9);
      oRM.attr("stoke", "black");
      oRM.attr("stoke-width", 2);
      oRM.attr("fill", oControl.getColor());
      oRM.write("/>");
      oRM.write("</svg>");

      oRM.close("div");
    },
  });
});
