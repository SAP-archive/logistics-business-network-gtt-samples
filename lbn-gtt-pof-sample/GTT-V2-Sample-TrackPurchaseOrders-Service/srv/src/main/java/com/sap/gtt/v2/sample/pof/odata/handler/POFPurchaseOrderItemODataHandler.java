package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.Event;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemInboundDeliveryItemTP;
import com.sap.gtt.v2.sample.pof.service.LocationService;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.stereotype.Component;
import org.springframework.web.util.UriComponentsBuilder;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderODataHandler.COMMA;
import static com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderODataHandler.COMMA_ENCODED;
import static com.sap.gtt.v2.sample.pof.odata.handler.POFPurchaseOrderODataHandler.DIV_ENCODED;
import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

@Component
public class POFPurchaseOrderItemODataHandler extends POFDefaultODataHandler {

    private static final String REGEX_LEADING_ZERO = "^0*";
    private static final String PROCESS_ID_FILTER_PART = " process_id eq guid'%s' ";
    private static final String PROCESS_EVENT_DIRECTORY_URI = "/ProcessEventDirectory";
    private static final String DIRECTORY_QUERY_TEMPLATE = "$expand=event" +
            "&$filter=(event/eventType eq 'com.lbngttsamples.gtt.app.pof.PurchaseOrderItem.DeletionEvent' " +
            "or event/eventType eq 'com.lbngttsamples.gtt.app.pof.PurchaseOrderItem.UndeletionEvent') and";
    public static final String DIV = "/";

    private final LocationService locationService;
    private final POFInboundDeliveryItemODataHandler pofInboundDeliveryItemODataHandler;


    public POFPurchaseOrderItemODataHandler(POFInboundDeliveryItemODataHandler pofInboundDeliveryItemODataHandler,
                                            LocationService locationService) {
        this.locationService = locationService;
        this.pofInboundDeliveryItemODataHandler = pofInboundDeliveryItemODataHandler;
    }

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Boolean isLocation = isLocationExists(uri);
        uri = removeUnnecessaryExpands(uri);

        ODataResultList<PurchaseOrderItem> entityList = gttCoreServiceClient.readEntitySet(uri, PurchaseOrderItem.class);
        List<PurchaseOrderItem> list = entityList.getResults();
        if (!list.isEmpty()) {
            removeUnneededLeadingZero(list);
            updateCompletionValues(list);
        }
        if (isLocation) {
            setLocations(entityList);
        }
        return convertResults(entityList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Boolean isLocation = isLocationExists(uri);
        uri = removeUnnecessaryExpands(uri);

        PurchaseOrderItem entity = gttCoreServiceClient.readEntity(uri, PurchaseOrderItem.class);
        removeUnneededLeadingZero(entity);
        removeNullInboundDeliveryItem(entity);
        updateCompletionValue(entity);

        if(isLocation) {
            locationService.setReceivingLocation(entity);
            locationService.setSupplierLocation(entity);
        }

        setLocationsForExpands(entity);

        return ODataUtils.toMap(entity);
    }

    public void updateCompletionValues(List<PurchaseOrderItem> entities) {
        StringBuilder queryBuilder = new StringBuilder(DIRECTORY_QUERY_TEMPLATE + "(");
        for (PurchaseOrderItem entity : entities) {
            String filter = String.format(PROCESS_ID_FILTER_PART + "or", entity.getId());
            queryBuilder.append(filter);
        }
        String query = queryBuilder.delete(queryBuilder.length() - 2, queryBuilder.length()).append(")").toString();
        String url = UriComponentsBuilder.fromUriString(PROCESS_EVENT_DIRECTORY_URI).query(query).build().encode().toUriString();
        List<ProcessEventDirectory> results = gttCoreServiceClient.readEntitySetAll(url, ProcessEventDirectory.class).getResults();
        Map<UUID, List<Event>> map = results.stream()
                .filter(ped -> nonNull(ped.getEvent()))
                .collect(Collectors.groupingBy(ProcessEventDirectory::getProcessId, Collectors.mapping(ProcessEventDirectory::getEvent, Collectors.toList())));
        for (PurchaseOrderItem purchaseOrderItem : entities) {
            List<Event> events = map.getOrDefault(purchaseOrderItem.getId(), Collections.emptyList());
            if (isDeletionLatest(events)) {
                purchaseOrderItem.setNetValue(BigDecimal.ZERO);
                purchaseOrderItem.setCompletionValue(BigDecimal.ZERO);
            }
        }
    }

    public void updateCompletionValue(PurchaseOrderItem entity) {
        String url = UriComponentsBuilder.fromUriString(PROCESS_EVENT_DIRECTORY_URI)
                .query(DIRECTORY_QUERY_TEMPLATE + String.format(PROCESS_ID_FILTER_PART, entity.getId()))
                .build().encode().toUriString();
        List<ProcessEventDirectory> results = gttCoreServiceClient.readEntitySetAll(url, ProcessEventDirectory.class).getResults();
        List<Event> events = results.stream().map(ProcessEventDirectory::getEvent).filter(Objects::nonNull).collect(Collectors.toList());
        if (isDeletionLatest(events)) {
            entity.setNetValue(BigDecimal.ZERO);
            entity.setCompletionValue(BigDecimal.ZERO);
        }
    }

    private boolean isDeletionLatest(List<Event> events) {
        return events.stream()
                .max(Comparator.comparingLong(Event::getActualBusinessTimestamp))
                .map(Event::getEventType).orElse(EMPTY).endsWith(".DeletionEvent");
    }

    private void setLocationsForExpands(PurchaseOrderItem purchaseOrderItem) {
        if (purchaseOrderItem.getInboundDeliveryItems() != null) {
            Map<String, LocationDTO> map = locationService.getLocationsForPurchaseOrderItemInboundDeliveryItem(purchaseOrderItem.getInboundDeliveryItems());
            purchaseOrderItem.getInboundDeliveryItems()
                    .stream()
                    .filter(x -> x.getInboundDeliveryItem() != null)
                    .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                    .forEach(inboundDeliveryItem -> locationService.setLocationsForInboundDelivery(inboundDeliveryItem, map));
        }
    }

    private void setLocations(ODataResultList<PurchaseOrderItem> entityList) {
        Map<String, LocationDTO> map = locationService.getLocationsForPurchaseOrderItems(entityList.getResults());
        entityList.getResults().forEach(purchaseOrderItem -> {
            locationService.setLocationsForPurchaseOrderItem(purchaseOrderItem, map);

            purchaseOrderItem.getInboundDeliveryItems()
                    .stream()
                    .filter(x -> x.getInboundDeliveryItem() != null)
                    .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                    .forEach(inboundDeliveryItem -> locationService.setLocationsForInboundDelivery(inboundDeliveryItem, map));
        });
    }

    private Boolean isLocationExists(String uri) {
        return uri.contains(Constants.RECEIVING_LOCATION) || uri.contains(Constants.SUPPLIER_LOCATION);
    }

    private String removeUnnecessaryExpands(String uri) {
        String urlWithoutExpands = "";
        if (uri.contains(COMMA)) {
            urlWithoutExpands = removeCommas(uri, COMMA, DIV);
        } else if (uri.contains(COMMA_ENCODED)) {
            urlWithoutExpands = removeCommas(uri, COMMA_ENCODED, DIV_ENCODED);
        }
        if (urlWithoutExpands.contains(Constants.RECEIVING_LOCATION)) {
            urlWithoutExpands = POFUtils.removeFieldFromUrl(urlWithoutExpands, Constants.RECEIVING_LOCATION);
        }
        if (urlWithoutExpands.contains(Constants.SUPPLIER_LOCATION)) {
            urlWithoutExpands = POFUtils.removeFieldFromUrl(urlWithoutExpands, Constants.SUPPLIER_LOCATION);
        }
        return urlWithoutExpands;
    }

    private String removeCommas(String uri, String comma, String div) {
        String urlWithoutExpands = "";
        String[] split = uri.split(comma);
        StringBuilder urlWithoutExpandsBuilder = new StringBuilder();
        for (String urlElement : split) {
            if (urlElement.contains(div + Constants.RECEIVING_LOCATION) ||
                    urlElement.contains(div + Constants.SUPPLIER_LOCATION)
                    || urlElement.contains(div + Constants.PLANT_LOCATION)) {
                continue;
            }

            urlWithoutExpandsBuilder
                    .append(urlElement)
                    .append(comma);
        }
        urlWithoutExpands = urlWithoutExpandsBuilder.toString();

        if (urlWithoutExpands.endsWith(comma)) {
            urlWithoutExpands = Optional.of(urlWithoutExpands)
                    .map(sStr -> sStr.substring(0, sStr.length() - comma.length()))
                    .orElse(urlWithoutExpands);
        }

        return urlWithoutExpands;
    }

    private void removeUnneededLeadingZero(List<PurchaseOrderItem> items) {
        for (PurchaseOrderItem purchaseOrderItem : items) {
            removeUnneededLeadingZero(purchaseOrderItem);
            removeNullInboundDeliveryItem(purchaseOrderItem);
        }
    }

    private void removeNullInboundDeliveryItem(PurchaseOrderItem purchaseOrderItem) {
        List<PurchaseOrderItemInboundDeliveryItemTP> inboundDeliveryItems = purchaseOrderItem.getInboundDeliveryItems();
        if (inboundDeliveryItems != null && inboundDeliveryItems.size() > 0) {
            purchaseOrderItem.setInboundDeliveryItems(inboundDeliveryItems
                    .stream()
                    .filter(x -> x.getInboundDeliveryItem() != null)
                    .collect(Collectors.toList()));
        }
    }

    public void removeUnneededLeadingZero(PurchaseOrderItem item) {
        if (isNotBlank(item.getPurchaseOrderNo())) {
            item.setPurchaseOrderNo(item.getPurchaseOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotBlank(item.getMaterialId())) {
            item.setMaterialId(item.getMaterialId().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotBlank(item.getItemNo())) {
            item.setItemNo(item.getItemNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }

        if (item.getInboundDeliveryItems() != null && item.getInboundDeliveryItems().size() > 0) {
            item.getInboundDeliveryItems()
                    .stream()
                    .filter(x -> x.getInboundDeliveryItem() != null)
                    .map(PurchaseOrderItemInboundDeliveryItemTP::getInboundDeliveryItem)
                    .forEach(pofInboundDeliveryItemODataHandler::removeUnneededLeadingZero);
        }
    }
}
