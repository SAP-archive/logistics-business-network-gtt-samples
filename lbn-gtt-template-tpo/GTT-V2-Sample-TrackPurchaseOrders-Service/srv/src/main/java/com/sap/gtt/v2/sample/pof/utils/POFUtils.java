package com.sap.gtt.v2.sample.pof.utils;

import static com.sap.gtt.v2.sample.pof.constant.Constants.DATE_TIME_PATTERN;
import static com.sap.gtt.v2.sample.pof.constant.Constants.EXPAND;
import static com.sap.gtt.v2.sample.pof.constant.Constants.URL_SPLITTER;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.FILTER;
import static com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient.ORDERBY;
import static org.apache.logging.log4j.util.Strings.EMPTY;
import static org.apache.logging.log4j.util.Strings.isBlank;
import static org.apache.logging.log4j.util.Strings.isNotBlank;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.OrderBy;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.pof.odata.filter.FilterExpressionBuilder;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.TimeZone;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpStatus;
import org.springframework.web.util.UriComponentsBuilder;

public class POFUtils {

    public static final String PORT_NUM = ":((443)|(80))";
    public static final String SEPARATOR = "/";
    public static final String SELECT = "\\$select=[^\\$]*";
    public static final String BLANK = "";
    public static final String ALT_KEY_HEADER = "xri://sap.com/id:";
    public static final String LOCATION = "Location";

    public static final String EARLY_REPORTED = "EARLY_REPORTED";
    public static final String REPORTED = "REPORTED";
    public static final String LATE_REPORTED = "LATE_REPORTED";
    public static final String UNPLANNED = "UNPLANNED";
    public static final String UNPLANNED_DELAYED = "UNPLANNED_DELAYED";
    public static final String UNPLANNED_ONTIME = "UNPLANNED_ONTIME";
    public static final String EQ = " eq '";

    private static final String DATE_TIME_FORMAT_PATTERN = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    private static final int MAX_FILTER_LENGTH_FOR_SPLIT = 8192;

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
            "Shipment.Decoupling"
    );

    private static final List<String> EVENT_TYPE_WHITE_LIST_FOR_EXECUTION_FLOW = Arrays.asList(
            "PurchaseOrderItem.GoodsReceipt",
            "InboundDeliveryItem.Picking",
            "InboundDeliveryItem.PutAway",
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
            "Shipment.FlightBooked",
            "Shipment.ManifestReady",
            "Shipment.ReceivedFromShipper",
            "Shipment.ConsigneeNotified"
    );

    private static final List<String> EVENT_TYPE_BLACK_LIST_IN_MAP = Arrays.asList(
            "Delay",
            "LocationUpdate",
            "LocationUpdateNew"
    );

    private POFUtils() {

    }

    public static boolean isEventTypesEqual(String eventName, String eventType) {
        return isNotBlank(eventType) && eventType.endsWith(eventName);
    }

    public static boolean isEventTypeInBlackList(String eventType) {
        return EVENT_TYPE_BLACK_LIST_IN_MAP.stream().anyMatch(typeName -> typeName.equals(eventType));
    }

    public static boolean isEventTypeInWhiteList(String eventType) {
        String[] parts = eventType.split("\\.");
        String eventName = parts[parts.length - 1];
        return EVENT_TYPE_WHITE_LIST_FOR_EXECUTION_FLOW.stream().anyMatch(typeName -> typeName.endsWith(eventName));
    }

    public static String getStringFromResource(String resourceFile) {
        String json;
        try (InputStream inputStream = new ClassPathResource(resourceFile).getInputStream()) {
            json = IOUtils.toString(inputStream, "UTF-8");
        } catch (IOException e) {
            throw new POFServiceException(e, HttpStatus.INTERNAL_SERVER_ERROR.value());
        }
        return json;
    }

    public static String getNormalizedUri(ODataContext oDataContext) {
        try {
            String requestUri = oDataContext.getPathInfo().getRequestUri().toString();
            String serviceRoot = oDataContext.getPathInfo().getServiceRoot().toString();
            return getNormalizedUri(requestUri, serviceRoot);

        } catch (ODataException e) {
            throw new POFServiceException(e, HttpStatus.INTERNAL_SERVER_ERROR.value());
        }
    }

    public static String removeFieldInUrl(String uri, String removeField) {
        String regex_destinationLocation = "((\\s){0,}((\\$expand=)|(\\$orderby=))(\\s){0,}("+removeField+")(\\s)*(&|$))|(%2c{1}(\\s){0,}("+removeField+"))|((\\s){0,}("+removeField+")(\\s)*%2c{1})";
        uri = uri.replaceAll(regex_destinationLocation, "");
        return uri;
    }

    public static String removeFieldFromUrl(String uri, String removeField) {
        String regex_destinationLocation = "((\\s){0,}((\\$expand=)|(\\$orderby=))(\\s){0,}("+removeField+")(\\s)*(&|$))|((,|%2c){1}(\\s){0,}("+removeField+"))|((\\s){0,}("+removeField+")(\\s)*(,|%2c){1})";
        uri = uri.replaceAll(regex_destinationLocation, "");
        return uri;
    }

    static String getNormalizedUri(String requestUri, String serviceRoot) {
        requestUri = requestUri.replaceAll(PORT_NUM, BLANK);
        serviceRoot = serviceRoot.replaceAll(PORT_NUM, BLANK);
        String uri = requestUri.replace(serviceRoot, SEPARATOR);
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
            throw new POFServiceException(e, HttpStatus.INTERNAL_SERVER_ERROR.value());
        }
    }

    public static boolean isEventTypeWithEstimatedArrival(String eventType) {
        return EVENT_TYPE_WHITE_LIST_FOR_ESTIMATED_ARRIVAL.stream()
                .anyMatch(eventType::endsWith);
    }

    public static String getTimeStr(Long timeLong) {
        if (timeLong == null) {
            return null;
        }
        SimpleDateFormat formatter = new SimpleDateFormat(DATE_TIME_FORMAT_PATTERN);
        Date data = new Date(timeLong);
        return formatter.format(data);
    }

    public static String generateUrl(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, List<String> expand, List<OrderBy> orderby) {
        return POFUtils.generateUrl(targetEntityName, filterConditions, andOr, false, false, expand, orderby);
    }

    public static String generateUrlWithCorrelationType(String targetEntityName, List<FilterCondition> filterConditions, BinaryOperator andOr, List<String> expand, List<OrderBy> orderby) {
        return POFUtils.generateUrl(targetEntityName, filterConditions, andOr, true, false, expand, orderby);
    }

    public static String generateUrl(String targetEntityName, String filter, List<FilterCondition> filterConditions, BinaryOperator andOr, boolean isAdmissableCorrelatrionType, boolean isAdmissableCorrelationTypeWithoutGeoEvent, List<String> expand, List<OrderBy> orderby) {
        String filterStr = generateFilter(filterConditions, andOr, isAdmissableCorrelatrionType, isAdmissableCorrelationTypeWithoutGeoEvent);
        String expandStr = generateExpand(expand);
        String orderbyStr = generateOrderBy(orderby);
        UriComponentsBuilder re = UriComponentsBuilder.fromUriString(URL_SPLITTER.concat(targetEntityName));
        if(StringUtils.isNotBlank(filterStr))
            re = re.queryParam(FILTER,filterStr + (isBlank(filter) ? EMPTY : filter));
        if(StringUtils.isNotBlank(expandStr))
            re = re.queryParam(EXPAND,expandStr);
        if(StringUtils.isNotBlank(orderbyStr))
            re = re .queryParam(ORDERBY,orderbyStr);
        return re.build().encode().toUriString();
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
        return re.build().encode().toUriString();
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
        filterEx = !StringUtils.isAnyBlank(filterCorrelationTypeCode, filterStr) ?
                filterEx.concat(filterStr).concat(Constants.AND).concat(filterCorrelationTypeCode) :
                filterEx.concat(filterStr).concat(filterCorrelationTypeCode);
        return filterEx;
    }

    public static String generateExpand(List<String> expands) {
        if (CollectionUtils.isEmpty(expands)) {
            return EMPTY;
        }
        String expandsEx = "";
        for (String expand : expands) {
            expandsEx = expandsEx.concat(expand).concat(" , ");
        }
        expandsEx = expandsEx.substring(0, expandsEx.lastIndexOf(','));
        return expandsEx;
    }

    public static String generateOrderBy(List<OrderBy> orderBy) {
        if (CollectionUtils.isEmpty(orderBy)) {
            return EMPTY;
        }
        String orderbyStr = "";
        for (OrderBy order : orderBy) {
            orderbyStr = orderbyStr.concat(order.getOrderField()).concat(Constants.BLANK).concat(order.getSequence()).concat(",");
        }
        orderbyStr = orderbyStr.substring(0, orderbyStr.lastIndexOf(','));
        return orderbyStr;
    }

    public static String getCorrelationTypeCodeFilterString(boolean isAdmissableCorrelationTypeCode, boolean isAdmissableCorrelationTypeWithoutGeoEvent) {
        StringBuilder filterCorrelationTypeCode = new StringBuilder();
        for (int i = 0; isAdmissableCorrelationTypeCode && i < admissableCorrelationTypeCode.length; i++) {
            filterCorrelationTypeCode.append(Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ)
                    .append(admissableCorrelationTypeCode[i]).append("' or");
        }
        for (int i = 0; isAdmissableCorrelationTypeWithoutGeoEvent && i < admissableCorrelationTypeCodeWithoutGeo.length; i++) {
            filterCorrelationTypeCode.append(Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ)
                    .append(admissableCorrelationTypeCodeWithoutGeo[i]).append("' or");
        }
        if (StringUtils.isNotBlank(filterCorrelationTypeCode.toString())) {
            filterCorrelationTypeCode = new StringBuilder(filterCorrelationTypeCode.substring(0, filterCorrelationTypeCode.lastIndexOf("or")));
            filterCorrelationTypeCode = new StringBuilder(" (".concat(filterCorrelationTypeCode.toString()).concat(") "));
        }
        return filterCorrelationTypeCode.toString();
    }

    public static String generateLocationAltKey(String party, String system, String locationType, String locationId) {
        return new StringBuilder(ALT_KEY_HEADER).append(party).append(":").append(system).append(":")
                .append(LOCATION).append(":").append(locationType).append(":").append(locationId)
                .toString();
    }

    public static String generateJsonStringFromBean(Object object) {
        return getGson().toJson(object);
    }

    public static Gson getGson() {
        return new Gson();
    }

    public static Long getDateTimeLong(String dateString) {
        Instant instant = Instant.parse(dateString);
        return instant.getEpochSecond();
    }

    public static String getTimeString() {
        return getUTCTimeString(System.currentTimeMillis());
    }

    public static String getTimeStr() {
        return getTimeStr(System.currentTimeMillis());
    }

    public static String getUTCTimeString(Long time) {
        SimpleDateFormat formatter = new SimpleDateFormat(DATE_TIME_PATTERN);
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date date = new Date(time);
        return formatter.format(date);
    }

    private static final String[] reportedCorrelationTypeCode = {
            EARLY_REPORTED,
            REPORTED,
            LATE_REPORTED,
    };

    public static String getReportedCorrelationTypeCode() {
        String filterCorrelationTypeCode = "";
        for (int i = 0; i < reportedCorrelationTypeCode.length; i++) {
            filterCorrelationTypeCode += Constants.BLANK + Constants.CORRELATION_TYPE_CODE + EQ + reportedCorrelationTypeCode[i] + "' or";
        }
        filterCorrelationTypeCode = filterCorrelationTypeCode.substring(0, filterCorrelationTypeCode.lastIndexOf("or"));
        filterCorrelationTypeCode = " (".concat(filterCorrelationTypeCode).concat(") ");
        return filterCorrelationTypeCode;
    }

    public static List<String> generateSplitLargeFilterExpr(String expression, String operator, List values) {
        if (values.isEmpty()) {
            return Collections.emptyList();
        }
        List<String> filters = new LinkedList<>();
        StringBuilder filterBuilder = new StringBuilder();
        for (Object value : values) {
            String filter = String.format("%s %s ", String.format(expression, value), operator);
            filterBuilder.append(filter);
            if (filterBuilder.length() >= MAX_FILTER_LENGTH_FOR_SPLIT) {
                String substring = filterBuilder.substring(0, filterBuilder.length() - operator.length() - 1);
                filters.add(substring);
                filterBuilder = new StringBuilder();
            }
        }
        if (filterBuilder.length() != 0) {
            String substring = filterBuilder.substring(0, filterBuilder.length() - operator.length() - 1);
            filters.add(substring);
        }
        return filters;
    }
}
