sap.ui.define(
  [
    "sap/base/strings/formatMessage",
  ], function (
    formatMessage
  ) {
    "use strict";

    var MapHelper = {};
    var keys = {
      "HERE": "key-for-tile-service-of-here",
      "OpenStreet": "39c0ee96f30349beaba4850a6be58b45", // test key (map be invalid in the future)
    };

    var LanguageMappings = {
      "de": "ger",
      "en": "eng",
      "en-US": "eng",
      "es": "spa",
      "fr": "fre",
      "pt": "por",
      "ru": "rus",
      "zh-CN": "chi",
    };

    /**
     * Set the map configuration including the tile service
     * Free openstreet: a.tile.openstreetmap.org/{LOD}/{X}/{Y}.png, tile.openstreetmap.org/{LOD}/{X}/{Y}.png
     * @param {sap.ui.vbm.GeoMap} map The visual business map
     * @param {[string]} provider The provider name such as HERE, OpenStreet
     */
    MapHelper.setMapConfiguration = function (map, provider) {
      var protocal = "https";
      var hereHost = "1.base.maps.ls.hereapi.com";
      var appLanguage = sap.ui.getCore().getConfiguration().getLanguageTag();
      var tileLanguage = LanguageMappings[appLanguage] || "eng";

      var configuration = {
        "MapProvider": [{
          "name": "HERE",
          "type": "",
          "description": "",
          "tileX": "256",
          "tileY": "256",
          "maxLOD": "22",
          "copyright": "",
          "Source": [{
            "id": "s1",
            "url": formatMessage("{0}://{1}/maptile/2.1/maptile/newest/normal.day/'{LOD}'/'{X}'/'{Y}'/256/png8?apikey={2}&lg={3}", protocal, hereHost, keys.HERE, tileLanguage),
          }],
        }, {
          "name": "OpenStreet",
          "type": "",
          "description": "",
          "tileX": "256",
          "tileY": "256",
          "maxLOD": "22",
          "copyright": "",
          "Source": [{
            "id": "s1",
            "url": formatMessage("{0}://maps.geoapify.com/v1/tile/osm-carto/'{LOD}'/'{X}'/'{Y}'.png?apiKey={1}", protocal, keys.OpenStreet),
          }],
        }],
        "MapLayerStacks": [{
          "name": "DEFAULT",
          "MapLayer": {
            "name": "layer1",
            "refMapProvider": provider || "OpenStreet",
            "opacity": "1.0",
            "colBkgnd": "RGB(255,255,255)",
          },
        }],
      };
      map.setMapConfiguration(configuration);
      map.setRefMapLayerStack("DEFAULT");
    };

    return MapHelper;
  }
);
