package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.LocationDTO;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItemTP;
import com.sap.gtt.v2.sample.pof.service.LocationService;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Objects.isNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

@Component
public class POFPurchaseOrderODataHandler extends POFDefaultODataHandler {
    private static final String REGEX_LEADING_ZERO = "^0*";
    public static final String DIV = "/";
    public static final String DIV_ENCODED = "%2f";
    public static final String COMMA = ",";
    public static final String COMMA_ENCODED = "%2c";


    private final LocationService locationService;
    private final POFPurchaseOrderItemODataHandler purchaseOrderItemODataHandler;


    public POFPurchaseOrderODataHandler(LocationService locationService, POFPurchaseOrderItemODataHandler purchaseOrderItemODataHandler) {
        this.locationService = locationService;
        this.purchaseOrderItemODataHandler = purchaseOrderItemODataHandler;
    }

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Boolean isLocation = isLocationExists(uri);
        uri = removeUnnecessaryExpands(uri);

        ODataResultList<PurchaseOrder> entityList = gttCoreServiceClient.readEntitySet(uri, PurchaseOrder.class);

        if (isLocation) {
            setLocations(entityList);
        }
        removeUnneededLeadingZero(entityList.getResults());
        updateNetValuesForExpands(entityList.getResults());
        return convertResults(entityList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Boolean isLocation = isLocationExists(uri);

        uri = removeUnnecessaryExpands(uri);

        PurchaseOrder entity = gttCoreServiceClient.readEntity(uri, PurchaseOrder.class);

        if (isLocation) {
            locationService.setReceivingLocation(entity);
            locationService.setSupplierLocation(entity);
        }
        updateNetValuesForExpand(entity);
        setLocationsForExpands(entity);
        removeNullPurchaseOrderItem(entity);
        removeUnneededLeadingZero(entity);
        return ODataUtils.toMap(entity);
    }

    private void removeNullPurchaseOrderItem(PurchaseOrder purchaseOrder) {
        List<PurchaseOrderItemTP> purchaseOrderItemTPS = purchaseOrder.getPurchaseOrderItemTPs();
        if (purchaseOrderItemTPS != null && purchaseOrderItemTPS.size() > 0) {
            purchaseOrder.setPurchaseOrderItemTPs(purchaseOrderItemTPS
                    .stream()
                    .filter(x -> x.getPurchaseOrderItem() != null)
                    .collect(Collectors.toList()));
        }
    }

    public void updateNetValuesForExpands(List<PurchaseOrder> purchaseOrders) {
        List<PurchaseOrderItem> purchaseOrderItems = purchaseOrders.stream()
                .flatMap(it -> isNull(it.getPurchaseOrderItemTPs()) ? Stream.empty() : it.getPurchaseOrderItemTPs().stream())
                .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        if (!purchaseOrderItems.isEmpty()) {
            purchaseOrderItemODataHandler.updateCompletionValues(purchaseOrderItems);
        }
    }

    public void updateNetValuesForExpand(PurchaseOrder purchaseOrder) {
        if (purchaseOrder.getPurchaseOrderItemTPs() == null) {
            return;
        }
        List<PurchaseOrderItem> purchaseOrderItems = purchaseOrder.getPurchaseOrderItemTPs().stream()
                .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        if (!purchaseOrderItems.isEmpty()) {
            purchaseOrderItemODataHandler.updateCompletionValues(purchaseOrderItems);
        }
    }

    private void setLocationsForExpands(PurchaseOrder purchaseOrder) {
        if (purchaseOrder.getPurchaseOrderItemTPs() != null) {
            Map<String, LocationDTO> map = locationService.getLocationsForPurchaseOrderTP(purchaseOrder.getPurchaseOrderItemTPs());

            purchaseOrder.getPurchaseOrderItemTPs()
                    .stream()
                    .filter(x -> x.getPurchaseOrderItem() != null)
                    .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                    .forEach(purchaseOrderItem -> {
                        locationService.setLocationsForPurchaseOrderItem(purchaseOrderItem, map);
                    });
        }
    }

    private void setLocations(ODataResultList<PurchaseOrder> entityList) {
        Map<String, LocationDTO> map = locationService.getLocationsForPurchaseOrders(entityList.getResults());
        entityList.getResults().forEach(purchaseOrder -> {
           locationService.setLocationsForPurchaseOrder(purchaseOrder, map);
        });
    }

    private Boolean isLocationExists(String uri) {
        return uri.contains(Constants.RECEIVING_LOCATION) || uri.contains(Constants.SUPPLIER_LOCATION);
    }

    private String removeCommas(String uri, String comma, String div) {
        String urlWithoutExpands = "";
        String[] split = uri.split(comma);
        StringBuilder urlWithoutExpandsBuilder = new StringBuilder();
        for (String urlElement : split) {
            if (urlElement.contains(div + Constants.RECEIVING_LOCATION) ||
                    urlElement.contains(div + Constants.SUPPLIER_LOCATION)) {
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

        if (uri.contains(Constants.SUPPLIER_LOCATION)) {
            urlWithoutExpands = POFUtils.removeFieldFromUrl(urlWithoutExpands, Constants.SUPPLIER_LOCATION);
        }
        return urlWithoutExpands;
    }

    private void removeUnneededLeadingZero(List<PurchaseOrder> orders) {
        for (PurchaseOrder purchaseOrder : orders) {
            removeUnneededLeadingZero(purchaseOrder);
            removeNullPurchaseOrderItem(purchaseOrder);
        }
    }

    private void removeUnneededLeadingZero(PurchaseOrder order) {
        if (isNotBlank(order.getPurchaseOrderNo())) {
            order.setPurchaseOrderNo(order.getPurchaseOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }

        if (order.getPurchaseOrderItemTPs() != null) {
            order.getPurchaseOrderItemTPs()
                    .stream()
                    .map(PurchaseOrderItemTP::getPurchaseOrderItem)
                    .forEach(this::removeUnneededLeadingZero);
        }
    }

    private void removeUnneededLeadingZero(PurchaseOrderItem purchaseOrderItem) {
        if (purchaseOrderItem == null) {
            return;
        }
        if (isNotEmpty(purchaseOrderItem.getPurchaseOrderNo())) {
            purchaseOrderItem.setPurchaseOrderNo(purchaseOrderItem.getPurchaseOrderNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotEmpty(purchaseOrderItem.getMaterialId())) {
            purchaseOrderItem.setMaterialId(purchaseOrderItem.getMaterialId().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
        if (isNotEmpty(purchaseOrderItem.getItemNo())) {
            purchaseOrderItem.setItemNo(purchaseOrderItem.getItemNo().replaceAll(REGEX_LEADING_ZERO, EMPTY));
        }
    }

}
