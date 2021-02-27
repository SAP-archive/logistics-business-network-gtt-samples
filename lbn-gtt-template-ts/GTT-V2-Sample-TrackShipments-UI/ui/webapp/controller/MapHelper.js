sap.ui.define(
  [
    "sap/base/strings/formatMessage",
    "../controller/BaseController",
    "../util/ServiceUtils",
    "../util/RestClient",
  ], function (
    formatMessage,
    BaseController,
    ServiceUtils,
    RestClient
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
     * Fetch the key of HERE map
     * @returns {Promise} The service promise
     */
    MapHelper.getHereMapKey = function () {
      var jsonService = ServiceUtils.getDataSource("restService");
      var url = ServiceUtils.getUrl(jsonService.uri.concat("/hereMapKey"));
      var request = RestClient.get(url);
      request.then(function (data) {
        keys.HERE = data.key;
      }, function (error) {
        BaseController.prototype.handleServerError(error);
      });

      return request;
    };

    /**
     * Set the map configuration including the tile service
     * Free openstreet: a.tile.openstreetmap.org/{LOD}/{X}/{Y}.png, tile.openstreetmap.org/{LOD}/{X}/{Y}.png
     * @param {sap.ui.vbm.GeoMap} map The visual business map
     * @param {[string]} provider The provider name such as HERE, OpenStreet
     */
    MapHelper.setMapConfiguration = function (map, provider) {
      var protocal = "https";
      var hereHost1 = "1.base.maps.ls.hereapi.com";
      var hereHost2 = "2.base.maps.ls.hereapi.com";
      var hereHost3 = "3.base.maps.ls.hereapi.com";
      var hereHost4 = "4.base.maps.ls.hereapi.com";
      var appLanguage = sap.ui.getCore().getConfiguration().getLanguageTag();
      var tileLanguage = LanguageMappings[appLanguage] || "eng";
      var servicePattern = "{0}://{1}/maptile/2.1/maptile/newest/normal.day/'{LOD}'/'{X}'/'{Y}'/256/png8?apikey={2}&lg={3}";

      var configuration = {
        "MapProvider": [{
          "name": "HERE",
          "type": "",
          "description": "",
          "tileX": "256",
          "tileY": "256",
          "maxLOD": "22",
          "copyright": "Copyright &copy; HERE",
          "Source": [hereHost1, hereHost2, hereHost3, hereHost4].map(function (hereHost, index) {
            return {
              "id": "server" + index,
              "url": formatMessage(servicePattern, protocal, hereHost, keys.HERE, tileLanguage),
            };
          }),
        }, {
          "name": "OpenStreet",
          "type": "",
          "description": "",
          "tileX": "256",
          "tileY": "256",
          "maxLOD": "22",
          "copyright": "Copyright &copy; OpenStreet",
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
