package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.Location;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.service.LocationService;
import com.sap.gtt.v2.sample.pof.service.MapService;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNoneBlank;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Component
public class POFInboundDeliveryItemODataHandler extends POFDefaultODataHandler {
    private static final String REGEX_LEADING_ZERO = "^0*";
    public static final String COM_LBNGTTSAMPLES_GTT_APP_POF_SHIPMENT_ARRIVAL = Constants.GTT_MODEL_NAMESPACE + ".Shipment.Arrival";
    public static final String ARRIVAL_TIMES = "arrivalTimes";

    @Autowired
    private LocationService locationService;

    @Autowired
    private MapService mapService;

    @Autowired
    private ArrivalTimeODataHandler timeODataHandler;

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        boolean isLocation = isLocationExists(uri);
        boolean containsArrivalTimes = uri.contains(ARRIVAL_TIMES);

        uri = removeUnnecessaryExpands(uri);
        ODataResultList<InboundDeliveryItem> entityList = gttCoreServiceClient.readEntitySet(uri, InboundDeliveryItem.class);

        if (isLocation) {
            setLocations(entityList);
        }
        if (containsArrivalTimes) {
            updateArrivalTime(entityList.getResults());
        }

        for (InboundDeliveryItem inboundDeliveryItem : entityList.getResults()) {
            updateLastLocationDescription(inboundDeliveryItem);
            updatePlannedArrivalAt(inboundDeliveryItem);
        }

        removeUnneededLeadingZero(entityList.getResults());

        return convertResults(entityList);
    }

    private Boolean isLocationExists(String uri) {
        return uri.contains(Constants.PLANT_LOCATION) || uri.contains(Constants.SUPPLIER_LOCATION);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Boolean isLocation = isLocationExists(uri);
        boolean containsArrivalTimes = uri.contains(ARRIVAL_TIMES);

        uri = removeUnnecessaryExpands(uri);
        InboundDeliveryItem entity = gttCoreServiceClient.readEntity(uri, InboundDeliveryItem.class);

        if (isLocation) {
            locationService.setPlantLocation(entity);
            locationService.setSupplierLocation(entity);
        }
        if (containsArrivalTimes) {
            updateArrivalTime(entity);
        }

        updateLastLocationDescription(entity);
        updatePlannedArrivalAt(entity);

        removeUnneededLeadingZero(entity);

        return ODataUtils.toMap(entity);
    }

    public void updateLastLocationDescription(InboundDeliveryItem inboundDeliveryItem) {
        String lastLocationAltKey = inboundDeliveryItem.getLastLocationAltKey();
        if (isNotBlank(lastLocationAltKey)) {
            Location location = gttCoreServiceClient.getLocation(lastLocationAltKey);
            if (location != null) {
                inboundDeliveryItem.setLastLocationDescription(location.getLocationDescription());
            } else {
                String locationId = lastLocationAltKey.substring(lastLocationAltKey.lastIndexOf(':') + 1);
                inboundDeliveryItem.setLastLocationDescription(locationId);
            }
        }
    }

    public void updateArrivalTime(InboundDeliveryItem deliveryItem) {
        String locationAltKey = POFUtils.generateLocationAltKey(deliveryItem.getPartyId(), deliveryItem.getLogicalSystem(),
                deliveryItem.getPlantLocationTypeCode(), deliveryItem.getPlant());
        List<ArrivalTime> arrivalTimes4DeliveryItem = timeODataHandler.getArrivalTimes4DeliveryItem(deliveryItem.getId(), locationAltKey);
        deliveryItem.setArrivalTimes(arrivalTimes4DeliveryItem);
    }

    public void updateArrivalTime(List<InboundDeliveryItem> inboundDeliveryItems) {
        Map<UUID, List<ArrivalTime>> times4DeliveryItems = timeODataHandler.getArrivalTimes4DeliveryItems(inboundDeliveryItems);
        inboundDeliveryItems.forEach(it -> {
            List<ArrivalTime> arrivalTimes = times4DeliveryItems.getOrDefault(it.getId(), Collections.emptyList());
            it.setArrivalTimes(arrivalTimes);
        });
    }

    public void updatePlannedArrivalAt(InboundDeliveryItem inboundDeliveryItem) {
        if (inboundDeliveryItem.getPlantLocation() != null) {
            mapService
                    .getPlannedEvents4DeliveryItem(inboundDeliveryItem.getId().toString())
                    .stream()
                    .filter(x -> COM_LBNGTTSAMPLES_GTT_APP_POF_SHIPMENT_ARRIVAL.equals(x.getEventType()))
                    .filter(x -> isNoneBlank(x.getLocationAltKey()) && x.getLocationAltKey().equals(inboundDeliveryItem.getPlantLocation().getLocationAltKey()))
                    .reduce((first, second) -> second)
                    .ifPresent(plannedEvent -> inboundDeliveryItem.setPlannedArrivalTimestamp(plannedEvent.getPlannedBusinessTimestamp()));
        }
    }

    private String removeUnnecessaryExpands(String uri) {
        if (uri.contains(Constants.PLANT_LOCATION)) {
            uri = POFUtils.removeFieldFromUrl(uri, Constants.PLANT_LOCATION);
        }
        if (uri.contains(Constants.SUPPLIER_LOCATION)) {
            uri = POFUtils.removeFieldFromUrl(uri, Constants.SUPPLIER_LOCATION);
        }
        if (uri.contains(ARRIVAL_TIMES)) {
            uri = POFUtils.removeFieldFromUrl(uri, ARRIVAL_TIMES);
        }
        return uri;
    }

    private void setLocations(ODataResultList<InboundDeliveryItem> entityList) {
        Map<String, LocationDTO> map = locationService.getLocationsForInboundDeliveryItem(entityList.getResults());
        entityList.getResults().forEach(inboundDeliveryItem -> locationService.setLocationsForInboundDelivery(inboundDeliveryItem, map));
    }

    public void removeUnneededLeadingZero(List<InboundDeliveryItem> items) {
        items.forEach(this::removeUnneededLeadingZero);
    }

    public void removeUnneededLeadingZero(InboundDeliveryItem item) {
        if (isNotBlank(item.getItemNo())) {
            item.setItemNo(item.getItemNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotBlank(item.getInboundDeliveryNo())) {
            item.setInboundDeliveryNo(item.getInboundDeliveryNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotBlank(item.getSupplier())) {
            item.setSupplier(item.getSupplier().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotBlank(item.getMaterialNumber())) {
            item.setMaterialNumber(item.getMaterialNumber().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
    }
}
