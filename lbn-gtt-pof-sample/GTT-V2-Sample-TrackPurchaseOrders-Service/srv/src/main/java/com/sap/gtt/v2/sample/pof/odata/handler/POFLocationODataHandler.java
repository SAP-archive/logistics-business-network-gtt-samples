package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.service.MapService;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Component
public class POFLocationODataHandler extends POFDefaultODataHandler {

    private final MapService mapService;

    public POFLocationODataHandler(MapService mapService) {
        this.mapService = mapService;
    }

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }


    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }

    public Location getLocation(String locationAltKey) {
        return gttCoreServiceClient.getLocation(locationAltKey);
    }

    public Map<String,LocationDTO> getLocations(Set<String> locationAltKeys) {
        List<Location> locations = gttCoreServiceClient.getLocations(locationAltKeys);
        Map<String, LocationDTO> map = new HashMap<>();
        locations.forEach(location -> map.put(location.getLocationAltKey(),mapService.getLocationDetail(location)));
        return map;
    }
}
