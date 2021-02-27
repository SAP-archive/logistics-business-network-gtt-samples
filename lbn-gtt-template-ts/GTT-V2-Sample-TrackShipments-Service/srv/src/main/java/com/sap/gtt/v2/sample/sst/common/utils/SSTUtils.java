package com.sap.gtt.v2.sample.sst.common.utils;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.sst.common.exception.InternalErrorException;
import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import org.apache.commons.io.IOUtils;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.exception.ODataException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.DATE_TIME_PATTERN;
import static com.sap.gtt.v2.sample.sst.common.constant.TrackedProcessEventStatus.*;
import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * {@link SSTUtils} is a common util class.
 *
 * @author Min Li
 */
public class SSTUtils {

    public static final String PORT_NUM = ":((443)|(80))";
    public static final String SEPARATOR = "/";
    public static final String SELECT_REGEX = "\\$select=[^$]*";
    public static final String EXPAND_REGEX = "\\$expand=[^$]*";
    public static final String BLANK = "";

    private SSTUtils() {
    }

    public static String getStringFromResource(final String resourceFile) {
        try (final InputStream inputStream = new ClassPathResource(resourceFile).getInputStream()) {
            return IOUtils.toString(inputStream, UTF_8);
        } catch (IOException e) {
            throw new InternalErrorException(e);
        }
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

    public static String getNormalizedUri(String requestUri, String serviceRoot) {
        requestUri = requestUri.replaceAll(PORT_NUM, BLANK);
        serviceRoot = serviceRoot.replaceAll(PORT_NUM, BLANK);
        String uri = requestUri.replace(serviceRoot, SEPARATOR);
        uri = uri.replaceAll(SELECT_REGEX, BLANK);
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
            throw new SSTServiceException(e);
        }
    }

    public static String getDateTimeString(final Long time) {
        if (nonNull(time)) {
            final SimpleDateFormat formatter = new SimpleDateFormat(DATE_TIME_PATTERN);
            final Date date = new Date(time);
            return formatter.format(date);
        }
        return null;
    }

    public static Long getDateTimeLong(final String dateString) {
        final Instant instant = Instant.parse(dateString);
        return instant.getEpochSecond();
    }

    public static String generateJsonStringFromBean(Object object) {
        return new Gson().toJson(object);
    }

    public static String removeParametersFromUrl(final String uri, final String... parametersNames) {
        String result = uri;
        for (String parameterName : parametersNames) {
            result = removeParameterFromUri(result, parameterName);
        }
        return result;
    }

    public static String getEventTypeShortName(final String eventTypeFullName) {
        return eventTypeFullName.substring(eventTypeFullName.lastIndexOf('.') + 1);
    }

    public static String removeParameterFromUri(final String uri, final String parameterName) {
        Pattern pattern = Pattern.compile(EXPAND_REGEX);
        Matcher matcher = pattern.matcher(uri);
        if (matcher.find()) {
            final String expandExpression = matcher.group(0);
            final String parameterRegex = "((\\$expand=" + parameterName + ")$)|" +
                    "(%2c|,)+(" + parameterName + ")|(" + parameterName + ")(%2c|,)+";
            final String replacedExpandExpression = expandExpression.replaceAll(parameterRegex, EMPTY);
            return uri.replace(expandExpression, replacedExpandExpression);
        }
        return uri;
    }

    public static boolean isPlannedEventNotBeenReported(final String eventStatusCode) {
        return PLANNED.name().equals(eventStatusCode)
                || OVERDUE.name().equals(eventStatusCode)
                || DELAYED.name().equals(eventStatusCode);
    }
}
