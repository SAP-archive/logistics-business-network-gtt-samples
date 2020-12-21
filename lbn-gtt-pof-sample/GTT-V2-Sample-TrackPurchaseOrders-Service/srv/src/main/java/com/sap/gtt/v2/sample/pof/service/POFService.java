package com.sap.gtt.v2.sample.pof.service;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.*;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import org.apache.commons.lang3.LocaleUtils;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.apache.olingo.odata2.api.annotation.edm.EdmNavigationProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.Constants.URL_SPLITTER;

@Service
public class POFService {

    private static final Logger logger = LoggerFactory.getLogger(POFService.class);
    public static final String ID = "id";
    public static final String VP = "VP";

    private final GTTCoreServiceClient gttCoreServiceClient;

    @Autowired
    public POFService(GTTCoreServiceClient gttCoreServiceClient) {
        this.gttCoreServiceClient = gttCoreServiceClient;
    }

    public String getUiAnnotation() {
        logger.info("Get UiAnnotation");
        String uiAnnotation = gttCoreServiceClient.getUiAnnotation();
        return validateAndReturn(uiAnnotation);
    }

    private String validateAndReturn(String uiAnnotation) {
        JsonObject jsonObject = new JsonObject();
        jsonObject.addProperty("key", uiAnnotation);
        return jsonObject.get("key").getAsString();
    }

    public List<PlannedEvent> getPlannedEvents4TP(UUID tpId) {
        String query = "/PlannedEvent";
        String filter = "$filter";
        String guidFilter = "process_id eq guid'" + tpId + "'";
        String uriString = UriComponentsBuilder.fromUriString(query).queryParam(filter, guidFilter).encode().toUriString();
        ODataResultList<PlannedEvent> result = gttCoreServiceClient.readEntitySetAll(uriString, PlannedEvent.class);
        return result.getResults();
    }

    public String getI18n(String properties) {
        logger.info("Get I18n for {}", properties);
        String rawI18n = gttCoreServiceClient.getI18n(properties);
        List<String> codeListsToAppend = Arrays.asList("EventStatus", "CarrierRefDocumentType");
        Locale locale = getLocaleFromProperties(properties);
        return appendCodeListI18n(rawI18n, codeListsToAppend, locale);
    }

    private Locale getLocaleFromProperties(String properties) {
        Locale defaultLocale = Locale.ENGLISH;
        int startIndex = properties.indexOf('_');
        int endIndex = properties.lastIndexOf('.');

        return startIndex == -1 ? defaultLocale : LocaleUtils.toLocale(properties.substring(startIndex + 1, endIndex));
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
                        throw new POFServiceException(e);
                    }
                });
                sb.append(System.lineSeparator());
            } catch (ClassNotFoundException e) {
                throw new POFServiceException(e);
            }
        });

        return sb.toString();
    }

    public List<CarrierRefDocumentForDeliveryItem> getCarrierRefDocuments(UUID deliveryItemId) {
        String query = "/InboundDeliveryItem(guid'{placeholder}')?$expand=inboundDelivery/shipmentTPs/shipment/carrierRefDocuments";
        query = query.replace("{placeholder}", deliveryItemId.toString());
        InboundDeliveryItem inboundDeliveryItem = gttCoreServiceClient.readEntity(query, InboundDeliveryItem.class);

        List<CarrierRefDocumentForDeliveryItem> res = new ArrayList<>();
        if (inboundDeliveryItem.getInboundDelivery() != null) {
            for (ShipmentTP shipmentTP : inboundDeliveryItem.getInboundDelivery().getShipmentTPs()) {
                Shipment shipment = shipmentTP.getShipment();
                if (shipment != null) {
                    List<CarrierRefDocument> carrierRefDocuments = shipment.getCarrierRefDocuments();
                    for (CarrierRefDocument carrierRefDocument : carrierRefDocuments) {
                        CarrierRefDocumentForDeliveryItem doc = new CarrierRefDocumentForDeliveryItem();
                        doc.setDocId(carrierRefDocument.getDocId());
                        doc.setDocTypeCode(carrierRefDocument.getDocTypeCode());
                        doc.setShipmentNo(shipment.getShipmentNo());
                        doc.setShipmentId(shipment.getId());
                        res.add(doc);
                    }

                    CarrierRefDocumentForDeliveryItem doc = new CarrierRefDocumentForDeliveryItem();
                    doc.setDocTypeCode(VP);
                    doc.setDocId(shipment.getTrackId());
                    doc.setShipmentNo(shipment.getShipmentNo());
                    doc.setShipmentId(shipment.getId());
                    res.add(doc);
                }
            }
        }

        return res;
    }
}
