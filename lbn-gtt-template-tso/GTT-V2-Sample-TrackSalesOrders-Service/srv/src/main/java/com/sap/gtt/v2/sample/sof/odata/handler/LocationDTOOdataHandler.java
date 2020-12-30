package com.sap.gtt.v2.sample.sof.odata.handler;


import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.sof.rest.controller.service.MapService;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.*;
import java.util.stream.Collectors;

@Component
public class LocationDTOOdataHandler extends SOFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(LocationDTOOdataHandler.class);
    @Autowired
    private MapService mapService;
    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }


    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }

    public void getLocationDTO4TP(DeliveryItem deliveryItem) {
        String destinationAltKey = SOFUtils.generateLocationAltKey(deliveryItem.getPartyId(),deliveryItem.getLogicalSystem(),deliveryItem.getDestinationLocationTypeCode(),deliveryItem.getDestination());
        deliveryItem.setDestinationLocation(getLocationDTO(destinationAltKey));
        deliveryItem.setDestinationAltKey(destinationAltKey);
    }

    public LocationDTO getLocationDTO(String locationAltKey) {
        Location location = gttCoreServiceClient.getLocation(locationAltKey);
        LocationDTO locationDTO = mapService.getLocationDetail(location);
        return locationDTO;
    }

    public Map<String,LocationDTO> getLocationDTOs(Set<String> LocationAltKeys) {
        List<Location> locations = gttCoreServiceClient.getLocations(LocationAltKeys);
        Map<String,LocationDTO> map = new HashMap<>();
        locations.forEach(location -> map.put(location.getLocationAltKey(),mapService.getLocationDetail(location)));
        return map;
    }
}
