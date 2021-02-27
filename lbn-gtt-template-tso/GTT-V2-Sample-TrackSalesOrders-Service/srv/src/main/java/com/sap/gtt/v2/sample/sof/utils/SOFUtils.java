package com.sap.gtt.v2.sample.sof.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.OrderBy;
import com.sap.gtt.v2.sample.sof.exception.InternalErrorException;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.core.io.ClassPathResource;
import org.springframework.util.CollectionUtils;
import org.springframework.web.util.UriComponentsBuilder;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import static com.sap.gtt.v2.sample.sof.constant.Constants.URL_SPLITTER;
import static com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient.*;
import static java.util.Objects.isNull;

public class SOFUtils {

    public static final String PORT_NUM = ":((443)|(80))";
    public static final String SEPARATOR = "/";
    public static final String SELECT = "\\$select=[^\\$]*";
    public static final String BLANK = "";
    public static final String ALT_KEY_HEADER = "xri://sap.com/id:";
    public static final String LOCATION = "Location";
    public static final String EQ = " eq '";

    private SOFUtils() {

    }

    private static final List<String> EVENT_TYPE_WHITE_LIST_FOR_EXECUTION_FLOW = Arrays.asList(
            "DeliveryItem.Picking",
            "DeliveryItem.Packing",
            "DeliveryItem.DeliveryItemPOD",
            "Delivery.GoodsIssued",
            "Shipment.Departure",
            "Shipment.Arrival",
            "Shipment.LoadingStart",
            "Shipment.LoadingEnd",
            "Shipment.POD",
            "Shipment.GateInStart",
            "Shipment.GateInEnd",
            "Shipment.GateOutStart",
            "Shipment.GateOutEnd",
            "Shipment.UnloadingStart",
            "Shipment.UnloadingEnd",
            "Shipment.Stuffing",
            "Shipment.UnStuffing",
            "Shipment.Return",
            "Shipment.POPU",
            "Shipment.CheckIn",
            "Shipment.Receive",
            "Shipment.Delivered",
            "Shipment.Coupling",
            "Shipment.Decoupling",
            "Shipment.OutForDelivery",
            "Shipment.OtherEvent",
            "Shipment.ExceptionalEvent");

    public static boolean isEventTypeInWhiteList(String eventType) {
        String[] parts = eventType.split("\\.");
        String eventName = parts[parts.length - 1];
        return EVENT_TYPE_WHITE_LIST_FOR_EXECUTION_FLOW.stream().anyMatch(typeName -> typeName.endsWith(eventName));
    }
    private static final List<String> EVENT_STATUS_WHITE_LIST_IN_MAP = Arrays.asList(
            "PLANNED",
            "OVERDUE",
            "DELAYED"
            );

    public static boolean isEventStatusInWhiteList(String eventStatus) {
        return EVENT_STATUS_WHITE_LIST_IN_MAP.stream().anyMatch(typeName -> typeName.equals(eventStatus));
    }

    private static final List<String> EVENT_TYPE_BLACK_LIST_IN_MAP = Arrays.asList(
            "Delay",
            "LocationUpdate"
    );

    public static boolean isEventTypeInBlackList(String eventType) {
        return EVENT_TYPE_BLACK_LIST_IN_MAP.stream().anyMatch(typeName -> typeName.equals(eventType));
    }

    private static final List<String> EVENT_TYPE_WHITE_LIST_FOR_ESTIMATED_ARRIVAL = Arrays.asList(
            "Shipment.Departure",
            "Shipment.Arrival",
            "Shipment.LoadingStart",
            "Shipment.LoadingEnd",
            "Shipment.POD",
            "Shipment.GateInStart",
            "Shipment.GateInEnd",
            "Shipment.GateOutStart",
            "Shipment.GateOutEnd",
            "Shipment.UnloadingStart",
            "Shipment.UnloadingEnd",
            "Shipment.Stuffing",
            "Shipment.LocationUpdate",
            "Shipment.Delay",
            "Shipment.UnStuffing",
            "Shipment.Return",
            "Shipment.POPU",
            "Shipment.Receive",
            "Shipment.Delivered",
            "Shipment.Coupling",
            "Shipment.Decoupling");

    public static boolean isEventTypeWithEstimatedArrival(String eventType) {
        return EVENT_TYPE_WHITE_LIST_FOR_ESTIMATED_ARRIVAL.stream().anyMatch(typeName -> eventType.endsWith(typeName));
    }

    public static String getStringFromResource(String resourceFile) {
        String json;
        try (InputStream inputStream = new ClassPathResource(resourceFile).getInputStream()) {
            json = IOUtils.toString(inputStream);
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
        return json;
    }

    public static String getNormalizedUri(ODataContext oDataContext) {
        try {
            String requestUri = oDataContext.getPathInfo().getRequestUri().toString();
            String serviceRoot = oDataContext.getPathInfo().getServiceRoot().toString();
            return getNormalizedUri(requestUri, serviceRoot);

        } catch (ODataException e) {
            throw new InternalErrorException(e);
        }
    }

    static String getNormalizedUri(String requestUri, String serviceRoot) {
        requestUri = requestUri.replaceAll(PORT_NUM, BLANK);
        serviceRoot = serviceRoot.replaceAll(PORT_NUM, BLANK);
        String uri =requestUri.replace(serviceRoot, SEPARATOR);
        uri = uri.replaceAll(SELECT, BLANK);
        return uri;
    }

    public static String getPrettyJsonString(String json) {
        Gson gson = (new GsonBuilder().setPrettyPrinting()).create();
        JsonObject jsonObject = gson.fromJson(json, JsonObject.class);
        return gson.toJson(jsonObject);
    }

    public static String getTargetName(EdmType edmType) {
        try {
            return edmType.getNamespace() + "." + edmType.getName();
        } catch (EdmException e) {
            throw new SOFServiceException(e);
        }
    }

    public static String generateLocationAltKey(String party, String system, String locationType, String locationId) {
        return new StringBuilder(ALT_KEY_HEADER).append(party).append(":").append(system).append(":")
                .append(LOCATION).append(":").append(locationType).append(":").append(locationId)
                .toString();
    }

    public static String generateFilter(List<FilterCondition> conditions, BinaryOperator andOr, boolean isAdmissableCorrelationTypeCode, boolean isAdmissableCorrelationTypeWithoutGeoEvent) {
        String filterEx = "";
        FilterExpression filterExpression = FilterExpressionBuilder.createFilterExpression(conditions, andOr);
        String filterStr = filterExpression.getExpressionString();
        if (StringUtils.isNotBlank(filterStr)) {
            filterStr = " (" + filterStr + ") ";
        }
        String filterCorrelationTypeCode = getCorrelationTypeCodeFilterString(isAdmissableCorrelationTypeCode, isAdmissableCorrelationTypeWithoutGeoEvent);
        if (StringUtils.isAllBlank(filterStr, filterCorrelationTypeCode)) {
            return "";
        }
        if (!StringUtils.isAnyBlank(filterCorrelationTypeCode, filterStr)) {
            filterEx = filterEx.concat(filterStr).concat(Constants.AND).concat(filterCorrelationTypeCode);
        } else {
            filterEx = filterEx.concat(filterStr).concat(filterCorrelationTypeCode);
        }
        return filterEx;
    }

    public static final String EARLY_REPORTED = "EARLY_REPORTED";
    public static final String REPORTED = "REPORTED";
    public static final String LATE_REPORTED = "LATE_REPORTED";
    public static final String UNPLANNED = "UNPLANNED";
    public static final String UNPLANNED_DELAYED = "UNPLANNED_DELAYED";
    public static final String UNPLANNED_ONTIME = "UNPLANNED_ONTIME";
    private static final String[] admissableCorrelationTypeCode = {
            EARLY_REPORTED,
            REPORTED,
            LATE_REPORTED,
            UNPLANNED,
            UNPLANNED_DELAYED,
            UNPLANNED_ONTIME
    };
    private static final String[] admissableCorrelationTypeCodeWithoutGeo = {
            EARLY_REPORTED,
            REPORTED,
            LATE_REPORTED,
            UNPLANNED
    };
    public static String getCorrelationTypeCodeFilterString(boolean isAdmissableCorrelationTypeCode, boolean isAdmissableCorrelationTypeWithoutGeoEvent) {
        String filterCorrelationTypeCode = "";
        for (int i = 0; isAdmissableCorrelationTypeCode && i < admissableCorrelationTypeCode.length; i++) {
            filterCorrelationTypeCode += Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ + admissableCorrelationTypeCode[i] + "' or";
        }
        for (int i = 0; isAdmissableCorrelationTypeWithoutGeoEvent && i < admissableCorrelationTypeCodeWithoutGeo.length; i++) {
            filterCorrelationTypeCode += Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ + admissableCorrelationTypeCodeWithoutGeo[i] + "' or";
        }
        if (StringUtils.isNotBlank(filterCorrelationTypeCode)) {
            filterCorrelationTypeCode = filterCorrelationTypeCode.substring(0, filterCorrelationTypeCode.lastIndexOf("or"));
            filterCorrelationTypeCode = " (".concat(filterCorrelationTypeCode).concat(") ");
        }
        return filterCorrelationTypeCode;
    }
    private static final String[] reportedCorrelationTypeCode = {
            EARLY_REPORTED,
            REPORTED,
            LATE_REPORTED,
    };
    public static String getReportedCorrelationTypeCode() {
        String filterCorrelationTypeCode = "";
        for (int i = 0;  i < reportedCorrelationTypeCode.length; i++) {
            filterCorrelationTypeCode += Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ + reportedCorrelationTypeCode[i] + "' or";
        }
        filterCorrelationTypeCode = filterCorrelationTypeCode.substring(0, filterCorrelationTypeCode.lastIndexOf("or"));
        filterCorrelationTypeCode = " (".concat(filterCorrelationTypeCode).concat(") ");
        return filterCorrelationTypeCode;
    }
    public static String generateUrl(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, List<String> expand, List<OrderBy> orderby) {
        return SOFUtils.generateUrl(targetEntityName, filterConditions, andOr, false, false, expand, orderby);
    }

    public static String generateUrlWithCorrelationType(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, List<String> expand, List<OrderBy> orderby) {
        return SOFUtils.generateUrl(targetEntityName, filterConditions, andOr, true, false, expand, orderby);
    }

    public static String generateUrlWithCorrelationTypeAndNoGeo(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, List<String> expand, List<OrderBy> orderby) {
        return SOFUtils.generateUrl(targetEntityName, filterConditions, andOr, false, true, expand, orderby);
    }

    public static String generateUrl(String targetEntityName, String filter, List<FilterCondition> filterConditions, BinaryOperator andOr, boolean isAdmissableCorrelatrionType, boolean isAdmissableCorrelationTypeWithoutGeoEvent, List<String> expand, List<OrderBy> orderby) {
        String filterStr = generateFilter(filterConditions, andOr, isAdmissableCorrelatrionType, isAdmissableCorrelationTypeWithoutGeoEvent);
        filterStr = isNull(filter) ? filterStr : filterStr.concat(filter);
        String expandStr = generateExpand(expand);
        String orderbyStr = generateOrderBy(orderby);
        UriComponentsBuilder re = UriComponentsBuilder.fromUriString(URL_SPLITTER.concat(targetEntityName));
        if(StringUtils.isNotBlank(filterStr))
            re = re.queryParam(FILTER,filterStr);
        if(StringUtils.isNotBlank(expandStr))
            re = re.queryParam(EXPAND,expandStr);
        if(StringUtils.isNotBlank(orderbyStr))
            re = re .queryParam(ORDERBY,orderbyStr);
        String url = re
                .build().encode().toUriString();
        return url;
    }

    public static String generateUrl(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, boolean isAdmissableCorrelatrionType, boolean isAdmissableCorrelationTypeWithoutGeoEvent, List<String> expand, List<OrderBy> orderby) {
        String filterStr = generateFilter(filterConditions, andOr, isAdmissableCorrelatrionType, isAdmissableCorrelationTypeWithoutGeoEvent);
        String expandStr = generateExpand(expand);
        String orderbyStr = generateOrderBy(orderby);
        UriComponentsBuilder re = UriComponentsBuilder.fromUriString(URL_SPLITTER.concat(targetEntityName));
        if(StringUtils.isNotBlank(filterStr))
            re = re.queryParam(FILTER,filterStr);
        if(StringUtils.isNotBlank(expandStr))
            re = re.queryParam(EXPAND,expandStr);
        if(StringUtils.isNotBlank(orderbyStr))
            re = re .queryParam(ORDERBY,orderbyStr);
        String url = re
                .build().encode().toUriString();
        return url;
    }

    public static String generateUrl(String targetEntityName, String filter, List<String> expand, List<OrderBy> orderby) {
        String expandStr = generateExpand(expand);
        String orderbyStr = generateOrderBy(orderby);
        UriComponentsBuilder re = UriComponentsBuilder.fromUriString(URL_SPLITTER.concat(targetEntityName));
        if(StringUtils.isNotBlank(filter))
            re = re.queryParam(FILTER,filter);
        if(StringUtils.isNotBlank(expandStr))
            re = re.queryParam(EXPAND,expandStr);
        if(StringUtils.isNotBlank(orderbyStr))
            re = re .queryParam(ORDERBY,orderbyStr);
        String url = re
                .build().encode().toUriString();
        return url;
    }

    public static String generateUrl(String targetEntityName, String filter, boolean isAdmissableCorrelationTypeWithoutGeoEvent, boolean isAdmissableCorrelationType, List<String> expand, List<OrderBy> orderby) {
        String correlationTypeCodeFilterString = getCorrelationTypeCodeFilterString(isAdmissableCorrelationType, isAdmissableCorrelationTypeWithoutGeoEvent);
        String expandStr = generateExpand(expand);
        String orderbyStr = generateOrderBy(orderby);
        UriComponentsBuilder re = UriComponentsBuilder.fromUriString(URL_SPLITTER.concat(targetEntityName));
        if(StringUtils.isNotBlank(filter.concat(correlationTypeCodeFilterString)))
            re = re.queryParam(FILTER,filter.concat(correlationTypeCodeFilterString));
        if(StringUtils.isNotBlank(expandStr))
            re = re.queryParam(EXPAND,expandStr);
        if(StringUtils.isNotBlank(orderbyStr))
            re = re .queryParam(ORDERBY,orderbyStr);
        String url = re
                .build().encode().toUriString();
        return url;
    }

    public static String generateExpand(List<String> expands) {
        if (CollectionUtils.isEmpty(expands)) {
            return "";
        }
        String expandsEx = "";
        for (String expand : expands) {
            expandsEx = expandsEx.concat(expand).concat(" , ");
        }
        expandsEx = expandsEx.substring(0, expandsEx.lastIndexOf(","));
        return expandsEx;
    }

    public static String generateOrderBy(List<OrderBy> orderBy) {
        if (CollectionUtils.isEmpty(orderBy)) {
            return "";
        }
        String orderbyStr = "";
        for (OrderBy order : orderBy) {
            orderbyStr = orderbyStr.concat(order.getOrderField()).concat(Constants.BLANK).concat(order.getSequence()).concat(",");
        }
        orderbyStr = orderbyStr.substring(0, orderbyStr.lastIndexOf(","));
        return orderbyStr;
    }

    public static String getTimeStr() {
        return getTimeStr(System.currentTimeMillis());
    }

    public static String getTimeStr(Long timeLong) {
        if (timeLong == null) {
            return null;
        }
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date data = new Date(timeLong);
        return formatter.format(data);
    }

    public static String removeFieldInUrl(String uri,String removeField) {
        String regex_destinationLocation = "((\\s){0,}((\\$expand=)|(\\" + GTTCoreServiceClient.ORDERBY + "=))(\\s){0,}(" +removeField+")(\\s)*(&|$))|(%2c{1}(\\s){0,}("+removeField+"))|((\\s){0,}("+removeField+")(\\s)*%2c{1})";
        uri = uri.replaceAll(regex_destinationLocation, "");
        return uri;
    }
    public static String generateJsonStringFromBean(Object object) {
        return getGson().toJson(object);
    }

    public static Gson getGson() {
        // TODO: add adapters and use singleton when necessary
        return new Gson();
    }
}
