package com.sap.gtt.v2.sample.sof.odata.handler;


import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class DeliveryItemOdataHandler extends SOFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(DeliveryItemOdataHandler.class);
    public static final String DESTINATION_LOCATION = "destinationLocation";
    public static final String ARRIVAL_TIMES = "arrivalTimes";

    @Autowired
    private ArrivalTimeOdataHandler arrivalTimeOdataHandler;
    @Autowired
    private LocationDTOOdataHandler locationDTOOdataHandler;
    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        boolean containsArrivalTimes = uri.contains(ARRIVAL_TIMES);
        if(containsArrivalTimes) {
            uri = SOFUtils.removeFieldInUrl(uri,ARRIVAL_TIMES);
        }
        ODataResultList<DeliveryItem> entityList = gttCoreServiceClient.readEntitySet(uri, DeliveryItem.class);
        if(containsArrivalTimes) {
            arrivalTimeOdataHandler.getArrivalTimes4DeliveryItems(entityList.getResults());
        }
        for (DeliveryItem deliveryItem : entityList.getResults()) {
            updateLastLocationDescription(deliveryItem);
        }
        return convertResults(entityList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        boolean containsArrivalTimes = uri.contains(ARRIVAL_TIMES);
        if(containsArrivalTimes) {
            uri = SOFUtils.removeFieldInUrl(uri,ARRIVAL_TIMES);
        }
        boolean containsDestinationLocation = uri.contains(DESTINATION_LOCATION);
        if(containsDestinationLocation) {
            uri = SOFUtils.removeFieldInUrl(uri,DESTINATION_LOCATION);
        }
        DeliveryItem deliveryItem = gttCoreServiceClient.readEntity(uri, DeliveryItem.class);
        if(containsArrivalTimes) {
            arrivalTimeOdataHandler.getArrivalTimes4DeliveryItem(deliveryItem.getId(),deliveryItem);
        }
        if(containsDestinationLocation&&deliveryItem!=null) {
            locationDTOOdataHandler.getLocationDTO4TP(deliveryItem);
        }
        updateLastLocationDescription(deliveryItem);

        return ODataUtils.toMap(deliveryItem);
    }

    private void updateLastLocationDescription(DeliveryItem deliveryItem) {
        if (deliveryItem == null) {
            return;
        }

        if (StringUtils.isNotEmpty(deliveryItem.getLastLocationAltKey())) {
            Location location = gttCoreServiceClient.getLocation(deliveryItem.getLastLocationAltKey());
            if(location!=null) {
                deliveryItem.setLastLocationDescription(location.getLocationDescription());
            }
        }
    }
}
