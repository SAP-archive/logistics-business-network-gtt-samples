package com.sap.gtt.v2.sample.sof.service;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.*;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.*;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.CarrierRefDocumentForDeliveryItem;
import com.sap.gtt.v2.sample.sof.rest.controller.domain.deliveryitem.FulfillmentStatus;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.lang3.LocaleUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.*;

import static com.sap.gtt.v2.sample.sof.constant.Constants.*;

@Service
public class SOFService {

    private static final Logger logger = LoggerFactory.getLogger(SOFService.class);

    public static final String TRACKED_PROCESS = "trackedProcess";
    public static final String SALES_ORDER_ITEM_ID = "salesOrderItem_id";
    public static final String ACTUAL_EVENT = "actualEvent";
    public static final String EVENT_TYPE = "eventType";
    public static final String SHIPMENT_DELAY = GTT_MODEL_NAMESPACE + ".Shipment.Delay";
    public static final String SHIPMENT_LOCATION_UPDATE = GTT_MODEL_NAMESPACE + ".Shipment.LocationUpdate";
    public static final String VP = "VP";
    public static final String LAST_CORRELATED_EVENT_ID = "lastCorrelatedEventId";
    public static final String ID = "id";
    public static final String ET = "ET_";
    public static final String DESCR = "_DESCR=";
    public static final String LINE_ENDING = "\\r?\\n";
    public static final String ALT_KEY = "altKey";
    public static final String DELIVERY_NO = "deliveryNo";
    public static final String ITEM_NO = "itemNo";

    @Autowired
    private GTTCoreServiceClient gttCoreServiceClient;

    public void executeTasks(String requestBodyFromForwarding) {
        logger.info("entering executeTasks...");
        JsonObject jsonObject = JsonParser.parseString(requestBodyFromForwarding).getAsJsonObject();
        JsonObject trackedProcess = jsonObject.getAsJsonObject(TRACKED_PROCESS);
        if (!trackedProcess.has(SALES_ORDER_ITEM_ID)) {
            return;
        }
        String salesOrderItemId = trackedProcess.get(SALES_ORDER_ITEM_ID).getAsString();

        JsonObject actualEvent = jsonObject.getAsJsonObject(ACTUAL_EVENT);
        String  eventType = actualEvent.get(EVENT_TYPE).getAsString();

        if (SHIPMENT_DELAY.equals(eventType) || SHIPMENT_LOCATION_UPDATE.equals(eventType) || SOFUtils.isEventTypeInWhiteList(eventType)) {
            updateCompletionAndDelayedQuantities(salesOrderItemId);
        }

        if (SOFUtils.isEventTypeInWhiteList(eventType)) {
            updateCompletedAndLateValues(salesOrderItemId);
            updateLastActivityOfDeliveryItem(trackedProcess);
        }
        logger.info("leaving executeTasks... salesOrderItemId:{}", salesOrderItemId);
    }


    public void updateLastActivityOfDeliveryItem(JsonObject trackedProcess) {
        String lastCorrelatedEventId = trackedProcess.get(LAST_CORRELATED_EVENT_ID).getAsString();
        String tpId = trackedProcess.get(ID).getAsString();
        if (StringUtils.isEmpty(lastCorrelatedEventId) || StringUtils.isEmpty(tpId)) {
            return;
        }
        String query = "/Event(guid'" + lastCorrelatedEventId + "')";
        Event event = gttCoreServiceClient.readEntity(query, Event.class);
        String eventType = event.getEventType().substring(event.getEventType().lastIndexOf(".")+1);
        query = "/" + eventType + "(guid'" + lastCorrelatedEventId +
                "')?$expand=eventProcesses/plannedEvent";

        EventEx actualEvent = gttCoreServiceClient.readEntity(query, EventEx.class);
        PlannedEvent plannedEvent = null;
        for (ProcessEventDirectory ped : actualEvent.getEventProcesses()) {
            if (UUID.fromString(tpId).equals(ped.getProcessId())) {
                plannedEvent = ped.getPlannedEvent();
                break;
            }
        }

        String lastEventName, lastVPLocationTypeCode , lastLocationAltKey = null;
        lastVPLocationTypeCode = actualEvent.getLocationTypeCode();
        if (plannedEvent != null) {
            lastLocationAltKey = plannedEvent.getLocationAltKey();
        }

        String eventName = actualEvent.getEventType();
        lastEventName = eventName.substring(eventName.lastIndexOf(".") + 1);

        String altKey = trackedProcess.get(ALT_KEY).getAsString();
        String deliveryNo = trackedProcess.get(DELIVERY_NO).getAsString();
        String itemNo = trackedProcess.get(ITEM_NO).getAsString();

        updateLastActivityOfDeliveryItem(lastEventName, lastVPLocationTypeCode, lastLocationAltKey, altKey, deliveryNo, itemNo);

    }

    public void updateLastActivityOfDeliveryItem(String lastEventName, String lastVPLocationTypeCode, String lastLocationAltKey, String altKey, String deliveryNo, String itemNo) {
        DeliveryItemEvent deliveryItemEvent = generateDeliveryItemEvent(altKey, deliveryNo, itemNo, lastEventName, lastVPLocationTypeCode, lastLocationAltKey);
        String body = new Gson().toJson(deliveryItemEvent);
        String writeUrl = "/DeliveryItemEvent";
        gttCoreServiceClient.write(body, writeUrl);
    }

    public DeliveryItemEvent generateDeliveryItemEvent(String altKey, String deliveryNo, String itemNo, String lastEventName, String lastVPLocationTypeCode, String lastLocationAltKey) {
        DeliveryItemEvent deliveryItemEvent = new DeliveryItemEvent();
        String time = SOFUtils.getTimeStr();
        deliveryItemEvent.setActualBusinessTimestamp(time);
        deliveryItemEvent.setActualBusinessTimeZone("UTC");
        deliveryItemEvent.setAltKey(altKey);
        deliveryItemEvent.setDeliveryNo(deliveryNo);
        deliveryItemEvent.setItemNo(itemNo);
        deliveryItemEvent.setLastEventName(lastEventName);
        deliveryItemEvent.setLastVPLocationTypeCode(lastVPLocationTypeCode);
        deliveryItemEvent.setLastLocationAltKey(lastLocationAltKey);

        return deliveryItemEvent;
    }

    public void updateCompletionAndDelayedQuantities(String salesOrderItemId) {
        String salesOrderId = updateQuantitiesInSalesOrderItem(salesOrderItemId);
        updateQuantitiesInSalesOrder(salesOrderId);
    }

    private String updateQuantitiesInSalesOrderItem(String salesOrderItemId) {
        String uri = "/SalesOrderItem(guid'" + salesOrderItemId + "')?$expand=deliveryItemTPs/deliveryItem";
        SalesOrderItem salesOrderItem = gttCoreServiceClient.readEntity(uri, SalesOrderItem.class);
        fillCalValInSalesOrderItem(salesOrderItem);
        SalesOrderItemEvent salesOrderItemEvent = generateSalesOrderItemEvent(salesOrderItem);
        String body = new Gson().toJson(salesOrderItemEvent);
        String writeUrl = "/SalesOrderItemEvent";
        gttCoreServiceClient.write(body, writeUrl);
        return salesOrderItem.getSalesOrderId();
    }

    private String updateQuantitiesInSalesOrder(String salesOrderId) {
        String uri = "/SalesOrder(guid'" + salesOrderId + "')?$expand=salesOrderItemTPs/salesOrderItem";
        SalesOrder salesOrder = gttCoreServiceClient.readEntity(uri, SalesOrder.class);
        fillCalValInSalesOrder(salesOrder);
        SalesOrderEvent salesOrderEvent = generateSalesOrderEvent(salesOrder);
        String body = new Gson().toJson(salesOrderEvent);
        String writeUrl = "/SalesOrderEvent";
        gttCoreServiceClient.write(body, writeUrl);
        return salesOrder.getId().toString();
    }
    private SalesOrderItemEvent generateSalesOrderItemEvent(SalesOrderItem salesOrderItem) {
        SalesOrderItemEvent salesOrderItemEvent = new SalesOrderItemEvent();
        salesOrderItemEvent.setAltKey(salesOrderItem.getAltKey());
        salesOrderItemEvent.setDelayedQuantity(salesOrderItem.getDelayedQuantity());
        salesOrderItemEvent.setCompletionQuantity(salesOrderItem.getCompletionQuantity());
        String time = SOFUtils.getTimeStr();
        salesOrderItemEvent.setActualBusinessTimestamp(time);
        salesOrderItemEvent.setActualBusinessTimeZone("UTC");
        salesOrderItemEvent.setSalesOrderNo(salesOrderItem.getSalesOrderNo());
        salesOrderItemEvent.setItemNo(salesOrderItem.getItemNo());
        salesOrderItemEvent.setDelayed(salesOrderItem.getDelayed());
        return salesOrderItemEvent;
    }

    private SalesOrderEvent generateSalesOrderEvent(SalesOrder salesOrder) {
        SalesOrderEvent salesOrderEvent = new SalesOrderEvent();
        salesOrderEvent.setAltKey(salesOrder.getAltKey());
        salesOrderEvent.setDelayedValue(salesOrder.getDelayedValue());
        salesOrderEvent.setCompletionValue(salesOrder.getCompletionValue());
        String time = SOFUtils.getTimeStr();
        salesOrderEvent.setActualBusinessTimestamp(time);
        salesOrderEvent.setActualBusinessTimeZone("UTC");
        salesOrderEvent.setSalesOrderNo(salesOrder.getSalesOrderNo());
        salesOrderEvent.setDelayed(salesOrder.getDelayed());
        return salesOrderEvent;
    }

    private void fillCalValInSalesOrderItem(SalesOrderItem salesOrderItem) {
        BigDecimal sumCompletionQuantity = BigDecimal.ZERO;
        BigDecimal sumDelayedQuantity = BigDecimal.ZERO;
        DeliveryItem deliveryItem;
        for (SalesOrderItemDeliveryItemTP deliveryItemTP : salesOrderItem.getDeliveryItemTPs()) {
            deliveryItem = deliveryItemTP.getDeliveryItem();
            if(deliveryItem == null) {
                continue;
            }
            sumDelayedQuantity = sumDelayedQuantity.add(delayedQuantityInDeliveryItem(deliveryItem));
            sumCompletionQuantity = sumCompletionQuantity.add(completionQuantityInSalesOrderItem(deliveryItem));
        }
        sumCompletionQuantity = sumCompletionQuantity.setScale(2,RoundingMode.HALF_UP);
        sumDelayedQuantity = sumDelayedQuantity.setScale(2,RoundingMode.HALF_UP);
        salesOrderItem.setDelayedQuantity(sumDelayedQuantity);
        salesOrderItem.setCompletionQuantity(sumCompletionQuantity);
        salesOrderItem.setDelayed(sumDelayedQuantity.compareTo(BigDecimal.ZERO) > 0);
    }

    private void fillCalValInSalesOrder(SalesOrder salesOrder) {
        BigDecimal singlecv;
        BigDecimal singledv;
        BigDecimal unitPrice;
        SalesOrderItem salesOrderItem;
        BigDecimal sumcv = new BigDecimal(0);
        BigDecimal sumdv = new BigDecimal(0);

        for (SalesOrderItemTP tp : salesOrder.getSalesOrderItemTPs()) {
            salesOrderItem = tp.getSalesOrderItem();
            if (salesOrderItem == null) {
                continue;
            }
            unitPrice = BigDecimal.ZERO.compareTo(salesOrderItem.getOrderQuantity()) == 0 ?
                    BigDecimal.ZERO : salesOrderItem.getNetValue().divide(salesOrderItem.getOrderQuantity(), MathContext.DECIMAL64);

            singlecv = salesOrderItem.getCompletionQuantity() == null ?
                    BigDecimal.ZERO : unitPrice.multiply(salesOrderItem.getCompletionQuantity());

            singledv = salesOrderItem.getDelayedQuantity() == null ?
                    BigDecimal.ZERO : unitPrice.multiply(salesOrderItem.getDelayedQuantity());

            sumcv = sumcv.add(singlecv);
            sumdv = sumdv.add(singledv);
        }

        sumcv = sumcv.setScale(2, RoundingMode.HALF_UP);
        sumdv = sumdv.setScale(2, RoundingMode.HALF_UP);

        salesOrder.setCompletionValue(sumcv);
        salesOrder.setDelayedValue(sumdv);
        salesOrder.setDelayed(sumdv.compareTo(BigDecimal.ZERO) > 0);
    }

    private BigDecimal delayedQuantityInDeliveryItem(DeliveryItem deliveryItem) {
        BigDecimal delayedQuantity = BigDecimal.ZERO;
        if (StringUtils.equals(deliveryItem.getProcessStatusCode(), Constants.PROCESS_STATUS_DELAYED)) {
            delayedQuantity = deliveryItem.getOrderQuantity() == null ? BigDecimal.ZERO :
                    deliveryItem.getOrderQuantity();
        }
        delayedQuantity = delayedQuantity.setScale(2,RoundingMode.HALF_UP);
        return delayedQuantity;
    }

    private BigDecimal completionQuantityInSalesOrderItem(DeliveryItem deliveryItem) {
        BigDecimal completionQuantity = BigDecimal.ZERO;
        if (StringUtils.equals(deliveryItem.getExecutionStatusCode(), EXECUTION_STATUS_COMPLETED)) {
            completionQuantity = deliveryItem.getOrderQuantity() == null ? BigDecimal.ZERO :
                    deliveryItem.getOrderQuantity();
        }
        completionQuantity = completionQuantity.setScale(2,RoundingMode.HALF_UP);
        return completionQuantity;
    }

    public void updateCompletedAndLateValues(String salesOrderItemId) {
        String uri = "/SalesOrderItem(guid'" + salesOrderItemId + "')?$expand=deliveryItemTPs/deliveryItem";
        SalesOrderItem salesOrderItem = gttCoreServiceClient.readEntity(uri, SalesOrderItem.class);

        updateCompletedAndLateQuantityOfSalesOrderItem(salesOrderItem);

        SalesOrder salesOrder = getSalesOrder(salesOrderItem);
        updateCompletedAndLateValueOfSalesOrder(salesOrder);
    }

    private boolean updateCompletedAndLateQuantityOfSalesOrderItem(SalesOrderItem salesOrderItem) {
        boolean isAllCompleted = true;
        BigDecimal sumQty = new BigDecimal(0);
        for (SalesOrderItemDeliveryItemTP deliveryItemTP : salesOrderItem.getDeliveryItemTPs()) {
            DeliveryItem deliveryItem = deliveryItemTP.getDeliveryItem();
            if (deliveryItem == null) {
                continue;
            }

            if (StringUtils.equals(deliveryItem.getExecutionStatusCode(), EXECUTION_STATUS_COMPLETED)) {
                if (StringUtils.equals(deliveryItem.getProcessStatusCode(), PROCESS_STATUS_LATE)) {
                    sumQty = sumQty.add(deliveryItem.getOrderQuantity());
                }

            } else {
                isAllCompleted = false;
            }
        }

        sumQty = sumQty.setScale(2, RoundingMode.HALF_UP);
        salesOrderItem.setCompletedAndLateQuantity(sumQty);
        salesOrderItem.setCompleted(isAllCompleted);

        SalesOrderItemEvent salesOrderItemEvent = generateSalesOrderItemEventForCompletedAndLateQuantity(salesOrderItem);
        String body = new Gson().toJson(salesOrderItemEvent);
        String writeUrl = "/SalesOrderItemEvent";
        gttCoreServiceClient.write(body, writeUrl);
        return isAllCompleted;
    }

    private void updateCompletedAndLateValueOfSalesOrder(SalesOrder salesOrder) {
        BigDecimal sumValue = new BigDecimal(0);
        boolean isAllCompleted = true;
        for (SalesOrderItemTP salesOrderItemTP : salesOrder.getSalesOrderItemTPs()) {
            SalesOrderItem salesOrderItem = salesOrderItemTP.getSalesOrderItem();
            if (salesOrderItem == null) continue;
            if (salesOrderItem.getNetValue() != null && salesOrderItem.getOrderQuantity() != null
                    && salesOrderItem.getCompletedAndLateQuantity() != null) {
                BigDecimal unitPrice = BigDecimal.ZERO.compareTo(salesOrderItem.getOrderQuantity()) == 0 ?
                        BigDecimal.ZERO : salesOrderItem.getNetValue().divide(salesOrderItem.getOrderQuantity(), MathContext.DECIMAL64);

                sumValue = sumValue.add(salesOrderItem.getCompletedAndLateQuantity().multiply(unitPrice));
            }
            if (!Objects.equals(salesOrderItem.getCompleted(), true)) {
                isAllCompleted = false;
            }
        }

        salesOrder.setCompleted(isAllCompleted);

        sumValue = sumValue.setScale(2, RoundingMode.HALF_UP);
        salesOrder.setCompletedAndLateValue(sumValue);
        SalesOrderEvent salesOrderEvent = generateSalesOrderEventForCompletedAndLateValue(salesOrder);
        String body = new Gson().toJson(salesOrderEvent);
        String writeUrl = "/SalesOrderEvent";
        gttCoreServiceClient.write(body, writeUrl);
    }

    private SalesOrder getSalesOrder(SalesOrderItem salesOrderItem) {
        String uri;
        uri = "/SalesOrder(guid'" + salesOrderItem.getSalesOrderId() + "')?$expand=salesOrderItemTPs/salesOrderItem";
        return gttCoreServiceClient.readEntity(uri, SalesOrder.class);
    }

    private SalesOrderItemEvent generateSalesOrderItemEventForCompletedAndLateQuantity(SalesOrderItem salesOrderItem) {
        SalesOrderItemEvent salesOrderItemEvent = new SalesOrderItemEvent();
        salesOrderItemEvent.setAltKey(salesOrderItem.getAltKey());
        salesOrderItemEvent.setCompletedAndLateQuantity(salesOrderItem.getCompletedAndLateQuantity());
        salesOrderItemEvent.setCompleted(salesOrderItem.getCompleted());

        String time = SOFUtils.getTimeStr();
        salesOrderItemEvent.setActualBusinessTimestamp(time);

        salesOrderItemEvent.setActualBusinessTimeZone("UTC");
        salesOrderItemEvent.setSalesOrderNo(salesOrderItem.getSalesOrderNo());
        salesOrderItemEvent.setItemNo(salesOrderItem.getItemNo());
        return salesOrderItemEvent;
    }

    private SalesOrderEvent generateSalesOrderEventForCompletedAndLateValue(SalesOrder salesOrder) {
        SalesOrderEvent salesOrderEvent = new SalesOrderEvent();
        salesOrderEvent.setAltKey(salesOrder.getAltKey());
        salesOrderEvent.setCompletedAndLateValue(salesOrder.getCompletedAndLateValue());
        salesOrderEvent.setCompleted(salesOrder.getCompleted());

        String time = SOFUtils.getTimeStr();
        salesOrderEvent.setActualBusinessTimestamp(time);

        salesOrderEvent.setActualBusinessTimeZone("UTC");
        salesOrderEvent.setSalesOrderNo(salesOrder.getSalesOrderNo());
        return salesOrderEvent;
    }

    public List<CarrierRefDocumentForDeliveryItem> getCarrierRefDocuments(UUID deliveryItemId) {
        String query = "/DeliveryItem(guid'{placeholder}')?$expand=delivery/shipmentTPs/shipment/carrierRefDocuments";
        query = query.replace("{placeholder}", deliveryItemId.toString());
        DeliveryItem deliveryItem = gttCoreServiceClient.readEntity(query, DeliveryItem.class);

        List<CarrierRefDocumentForDeliveryItem> res = new ArrayList<>();
        if (deliveryItem.getDelivery() != null) {
            for (ShipmentTP shipmentTP : deliveryItem.getDelivery().getShipmentTPs()) {
                Shipment shipment = shipmentTP.getShipment();
                if (shipment != null) {
                    List<CarrierRefDocument> carrierRefDocuments = shipment.getCarrierRefDocuments();
                    for (CarrierRefDocument carrierRefDocument : carrierRefDocuments) {
                        CarrierRefDocumentForDeliveryItem doc = new CarrierRefDocumentForDeliveryItem();
                        doc.setDocId(carrierRefDocument.getDocId());
                        doc.setDocTypeCode(carrierRefDocument.getDocTypeCode());
                        doc.setShipmentNo(shipment.getShipmentNo());
                        res.add(doc);
                    }

                    if (!StringUtils.isEmpty(shipment.getTrackID())) {
                        CarrierRefDocumentForDeliveryItem doc = new CarrierRefDocumentForDeliveryItem();
                        doc.setDocTypeCode(VP);
                        doc.setDocId(shipment.getTrackID());
                        doc.setShipmentNo(shipment.getShipmentNo());
                        res.add(doc);
                    }
                }
            }
        }

        return res;
    }

    public List<PlannedEvent> getPlannedEvents4TP(UUID tpId) {
        String query = "/PlannedEvent?$filter=process_id eq guid'" + tpId + "'";
        ODataResultList<PlannedEvent> result = gttCoreServiceClient.readEntitySetAll(query, PlannedEvent.class);
        return result.getResults();
    }

    public List<FulfillmentStatus> getFulfillmentStatus(UUID deliveryItemId) {
        ODataResultList<EventStatus> eventStatusList = gttCoreServiceClient.readEntitySetAll("/EventStatus?$inlinecount=allpages", EventStatus.class);
        Map<String, Integer> map = new HashMap<>();
        for (EventStatus eventStatus : eventStatusList.getResults()) {
            String eventStatusCode = eventStatus.getCode();
            if (EVENT_STATUS_EARLY_REPORTED.equals(eventStatusCode) ||
                    EVENT_STATUS_LATE_REPORTED.equals(eventStatusCode)) {
                eventStatusCode = EVENT_STATUS_REPORTED;
            }
            map.put(eventStatusCode, 0);
        }

        List<PlannedEvent> plannedEvents = getPlannedEvents4TP(deliveryItemId);
        for (PlannedEvent plannedEvent : plannedEvents) {
            String eventStatusCode = plannedEvent.getEventStatusCode();
            if (EVENT_STATUS_EARLY_REPORTED.equals(eventStatusCode) ||
                    EVENT_STATUS_LATE_REPORTED.equals(eventStatusCode)) {
                eventStatusCode = EVENT_STATUS_REPORTED;
            }
            map.put(eventStatusCode, map.getOrDefault(eventStatusCode, 0) + 1);
        }

        List<FulfillmentStatus> res = new ArrayList();
        for (String eventStatus : map.keySet()) {
            res.add(new FulfillmentStatus(eventStatus, map.get(eventStatus)));
        }

        return res;
    }

    public String getI18n(String properties) {
        String rawI18n = gttCoreServiceClient.getI18n(properties);
        List<String> codeListsToAppend = Arrays.asList("ExecutionStatus", "CarrierRefDocumentType", "VPLocationType");
        Locale locale = getLocaleFromProperties(properties);
        return appendCodeListI18n(rawI18n, codeListsToAppend, locale);
    }

    private String appendCodeListI18n(String rawI18n, List<String> codeListsToAppend, Locale locale) {
        StringBuilder sb = rawI18n == null ? new StringBuilder() : new StringBuilder(rawI18n);
        HttpHeaders headers = new HttpHeaders();
        headers.setAcceptLanguageAsLocales(Collections.singletonList(locale));

        codeListsToAppend.forEach(codeListName -> {
            try {
                String codeListPackageName = ProcessStatus.class.getPackage().getName();
                Class<?> codeListClazz = Class.forName(codeListPackageName + "." + codeListName);
                List<Field> navigationFields = FieldUtils.getFieldsListWithAnnotation(codeListClazz, EdmNavigationProperty.class);
                Class<?> codeListTextClazz = navigationFields.get(0).getDeclaredAnnotation(EdmNavigationProperty.class).toType();

                String query = URL_SPLITTER + codeListName + "?$expand=localized";
                List<?> list = gttCoreServiceClient.readEntitySetAll(query, codeListClazz, headers).getResults();

                list.forEach(entity -> {
                    try {
                        Method getLocalized = codeListClazz.getMethod("getLocalized");
                        Method getCode = codeListClazz.getMethod("getCode");
                        Method getName = codeListClazz.getMethod("getName");
                        Method getNameOfLocalized = codeListTextClazz.getMethod("getName");
                        Object localizedObj = codeListTextClazz.cast(getLocalized.invoke(entity));

                        if (localizedObj != null) {
                            sb.append(System.lineSeparator())
                                    .append("CO_")
                                    .append(entity.getClass().getSimpleName())
                                    .append("_")
                                    .append(getCode.invoke(entity))
                                    .append("_NAME=")
                                    .append(getNameOfLocalized.invoke(localizedObj));
                        } else {
                            sb.append(System.lineSeparator())
                                    .append("CO_")
                                    .append(entity.getClass().getSimpleName())
                                    .append("_")
                                    .append(getCode.invoke(entity))
                                    .append("_NAME=")
                                    .append(getName.invoke(entity));
                        }
                    } catch (NoSuchMethodException | IllegalAccessException| InvocationTargetException e) {
                        throw new SOFServiceException(e);
                    }
                });
                sb.append(System.lineSeparator());
            } catch (ClassNotFoundException e) {
                throw new SOFServiceException(e);
            }
        });

        return sb.toString();
    }

    private Locale getLocaleFromProperties(String properties) {
        Locale defaultLocale = Locale.ENGLISH;
        int startIndex = properties.indexOf('_');
        int endIndex = properties.lastIndexOf('.');

        // If parameter "properties" is i18n.properties, return Locale.ENGLISH by default
        return startIndex == -1 ? defaultLocale : LocaleUtils.toLocale(properties.substring(startIndex + 1, endIndex));
    }

}
