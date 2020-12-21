package com.sap.gtt.v2.sample.sst.common.client;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.COMBINED_MODEL_PATH_SEGMENT;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ELEMENTS_PATH_SEGMENT;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.GTT_MODEL_NAMESPACE;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.GTT_MODEL_NAMESPACE_WRITE_SERVICE;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;
import static com.sap.gtt.v2.sample.sst.common.constant.Constants.SHIPMENT_MODEL_NAME;
import static com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException.MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED;
import static com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException.MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED;
import static com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException.MESSAGE_CODE_CALL_READ_SERVICE_FAILED;
import static com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException.MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.ALL_PAGES;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.FILTER;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.FORMAT;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.INLINECOUNT;
import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.JSON;
import static java.lang.String.format;
import static java.util.Collections.singletonList;
import static java.util.Objects.nonNull;
import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;

import com.sap.gtt.v2.sample.sst.common.config.VcapParser;
import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import com.sap.gtt.v2.sample.sst.common.model.CodeListValue;
import com.sap.gtt.v2.sample.sst.common.model.Destination;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import javax.annotation.PostConstruct;
import javax.validation.constraints.NotNull;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

/**
 * {@link GTTCoreServiceClient} is a client which executes HTTP requests to external services.
 *
 * @author Min Li
 */
@Service
public class GTTCoreServiceClient {

    public static final String OUTBOUND_ODATA_V1 = "/outbound/odata/v1/";
    public static final String INBOUND_REST_V1 = "/inbound/rest/v1/";
    public static final String METADATA_V1_MODEL = "/metadata/v1/model/";
    public static final String INLINE_COUNT = "$inlinecount";
    public static final String INLINE_COUNT_ALL_PAGES = INLINE_COUNT + "=" + ALL_PAGES;
    public static final String INLINE_COUNT_NONE = "$inlinecount=none";
    public static final String SKIP = "&$skip=";
    public static final String LOCALIZED = "localized";
    public static final String LOCATION_V1 = "/location/v1";
    public static final String LOCATION = "/Location";
    public static final String LOCATION_V1_LOCATION = LOCATION_V1 + LOCATION;
    public static final String ADMISSIBLE_EVENT_TYPES = "admissibleEventTypes";
    public static final int TOO_MANY_RECORDS = 2000;

    private static final String LOCATION_ALT_KEY_PARAM = "locationAltKey";
    private static final Logger logger = LoggerFactory.getLogger(GTTCoreServiceClient.class);

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private VcapParser vcapParser;

    private String gttBaseUrl;
    private String techUser;
    private String criticalInfo;

    @Value("${DESTINATION_GTT:#{null}}")
    private String destinationGTT;

    @PostConstruct
    public void init() {
        Destination destination = vcapParser.getDestination(destinationGTT);
        gttBaseUrl = destination.getUrl();
        techUser = destination.getUser();
        criticalInfo = destination.getPassword();
    }

    public <T> ODataResultList<T> readEntitySetAll(String uri, Class<T> classOfT, HttpHeaders headers) {
        uri = StringUtils.replaceIgnoreCase(uri, INLINE_COUNT_NONE, INLINE_COUNT_ALL_PAGES);
        if (!StringUtils.containsIgnoreCase(uri, INLINE_COUNT_ALL_PAGES)) {
            uri += !uri.contains("?")
                    ? "?" + INLINE_COUNT_ALL_PAGES
                    : "&" + INLINE_COUNT_ALL_PAGES;
        }

        ODataResultList<T> res = readEntitySet(uri, classOfT, headers);
        if (nonNull(res.getCount()) && res.getCount() > TOO_MANY_RECORDS) {
            logger.warn("total count is: {}, url: {}", res.getCount(), uri);
        }

        while (nonNull(res.getCount()) && res.getCount() > res.getResults().size()) {
            String skip = SKIP + res.getResults().size();
            uri += skip;
            ODataResultList<T> tmp = readEntitySet(uri, classOfT, headers);
            res.setCount(tmp.getCount());
            res.getResults().addAll(tmp.getResults());
            uri = uri.replace(skip, EMPTY);
        }
        return res;
    }

    public <T> ODataResultList<T> readEntitySetAll(String uri, Class<T> classOfT) {
        return readEntitySetAll(uri, classOfT, null);
    }

    public <T> ODataResultList<T> readEntitySet(String uri, Class<T> classOfT) {
        String json = query(uri);
        return ODataUtils.readEntitySet(json, classOfT);
    }

    public <T> T readEntity(String uri, Class<T> classOfT) {
        String json = query(uri);
        return ODataUtils.readEntity(json, classOfT);
    }

    public <T> ODataResultList<T> readEntitySet(String uri, Class<T> classOfT, HttpHeaders headers) {
        String json = query(uri, headers);
        return ODataUtils.readEntitySet(json, classOfT);
    }

    public <T> T readEntity(String uri, Class<T> classOfT, HttpHeaders headers) {
        String json = query(uri, headers);
        return ODataUtils.readEntity(json, classOfT);
    }

    public Integer countEntitySet(String uri) {
        String json = query(uri);
        return Integer.parseInt(json);
    }

    public String query(String uri) {
        return query(uri, null);
    }

    public String query(String uri, HttpHeaders headers) {
        HttpHeaders finalHeaders = new HttpHeaders();
        if (nonNull(headers)) {
            finalHeaders.addAll(headers);
        }
        setBasicAuth(finalHeaders);
        if (finalHeaders.getAcceptLanguage().isEmpty()) {
            finalHeaders.setAcceptLanguageAsLocales(singletonList(LocaleContextHolder.getLocale()));
        }

        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, finalHeaders);
        final String url = buildReadServiceUrl(uri);

        logger.info("Ready to call read service: {}", url);

        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(url, GET, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call read service failed:", e);
            throw new SSTServiceException(MESSAGE_CODE_CALL_READ_SERVICE_FAILED);
        }
        return responseEntity.getBody();
    }

    public void write(String body, String uri) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<String> httpEntity = new HttpEntity<>(body, headers);
        final String url = buildWriteServiceUrl(uri);

        logger.info("Ready to call write service: {}", url);

        try {
            restTemplate.exchange(url, POST, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call write service failed:", e);
            throw new SSTServiceException(MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED);
        }
    }

    public String getUiAnnotation() {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildMetadataServiceUrl() + "/annotation";

        logger.info("Ready to call metadata service: {}", url);

        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(url, GET, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call metadata service failed:", e);
            throw new SSTServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
        return responseEntity.getBody();
    }

    public String getI18n(final String properties) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildMetadataServiceUrl() + "/i18n/" + properties;

        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(url, GET, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            throw new SSTServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
        return responseEntity.getBody();
    }

    public Optional<Location> getLocation(@NotNull final String altKey) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildLocationServiceWithAltKeyParam(altKey);

        logger.debug("Ready to call Location Service: {}", url);

        final ResponseEntity<String> response = restTemplate.exchange(url, GET, httpEntity, String.class);
        final List<Location> locations = ODataUtils.readEntitySet(response.getBody(), Location.class).getResults();
        return isEmpty(locations) ? Optional.empty() : Optional.of(locations.get(0));
    }

    public List<Location> getLocations() {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildLocationServiceUrl();

        logger.debug("Ready to call Location Service: {}", url);

        final ResponseEntity<String> response = restTemplate.exchange(url, GET, httpEntity, String.class);
        return ODataUtils.readEntitySet(response.getBody(), Location.class).getResults();
    }

    public ODataResultList<Location> getLocationsByUri(String uri) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildLocationServiceUrlByUri(uri);

        final ResponseEntity<String> response = restTemplate.exchange(url, GET, httpEntity, String.class);
        return ODataUtils.readEntitySet(response.getBody(), Location.class);
    }

    public Location getLocationByUri(final String uri) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildLocationServiceUrlByUri(uri);

        final ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(url, GET, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call Location service failed:", e);
            throw new SSTServiceException(MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED);
        }
        return ODataUtils.readEntity(responseEntity.getBody(), Location.class);
    }

    public String getEventTypesMetadata(final String eventType) {
        final HttpHeaders headers = buildHttpHeaders();
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildEventTypeMetadataServiceUrl(eventType);

        logger.debug("Ready to call Metadata Service: {}", url);

        try {
            final ResponseEntity<String> response = restTemplate.exchange(url, GET, httpEntity, String.class);
            return response.getBody();
        } catch (HttpStatusCodeException e) {
            throw new SSTServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
    }

    public List<CodeListValue> getCodeList(final String codeListName, final Locale locale) {
        final HttpHeaders headers = buildHttpHeaders();
        headers.setAcceptLanguageAsLocales(singletonList(locale));
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);
        final String url = buildCodeListUrl(codeListName);

        logger.debug("Ready to call Read Service: {}", url);

        try {
            final ResponseEntity<String> response = restTemplate.exchange(url, GET, httpEntity, String.class);
            return ODataUtils.readEntitySet(response.getBody(), CodeListValue.class).getResults();
        } catch (HttpStatusCodeException e) {
            throw new SSTServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
    }

    public String getUnplannedEventTypesOfTp(final String trackedProcess) {
        final String url = buildEventTypesOfTpServiceUrl(trackedProcess);
        final HttpHeaders headers = buildHttpHeaders();
        headers.setAcceptLanguageAsLocales(singletonList(LocaleContextHolder.getLocale()));
        final HttpEntity<Object> httpEntity = new HttpEntity<>(null, headers);

        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(url, GET, httpEntity, String.class);
        } catch (HttpStatusCodeException e) {
            throw new SSTServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
        return responseEntity.getBody();
    }

    private HttpHeaders buildHttpHeaders() {
        final HttpHeaders headers = new HttpHeaders();
        setBasicAuth(headers);
        return headers;
    }

    private void setBasicAuth(HttpHeaders headers) {
        headers.setBasicAuth(techUser, criticalInfo);
    }

    private String buildMetadataServiceUrl() {
        return UriComponentsBuilder.fromHttpUrl(gttBaseUrl + METADATA_V1_MODEL + GTT_MODEL_NAMESPACE)
                .build().encode().toUriString();
    }

    private String buildEventTypeMetadataServiceUrl(final String eventType) {
        final String eventModelNamespace = format("%s.%s", SHIPMENT_MODEL_NAME, eventType);
        return UriComponentsBuilder.fromHttpUrl(buildMetadataServiceUrl())
                .pathSegment(COMBINED_MODEL_PATH_SEGMENT, eventModelNamespace, ELEMENTS_PATH_SEGMENT)
                .build().encode().toUriString();
    }

    private String buildCodeListUrl(final String codeListName) {
        return UriComponentsBuilder.fromHttpUrl(buildReadServiceUrl("/" + codeListName))
                .queryParam(EXPAND, LOCALIZED)
                .build().encode().toUriString();
    }

    private String buildReadServiceUrl(final String uri) {
        return UriComponentsBuilder
                .fromHttpUrl(gttBaseUrl + OUTBOUND_ODATA_V1 + MODEL_NAMESPACE + uri)
                .build().toUriString();
    }

    private String buildWriteServiceUrl(final String uri) {
        return UriComponentsBuilder
                .fromHttpUrl(gttBaseUrl + INBOUND_REST_V1 + GTT_MODEL_NAMESPACE_WRITE_SERVICE + uri)
                .build().encode().toUriString();
    }

    private String buildLocationServiceUrl() {
        return UriComponentsBuilder
                .fromHttpUrl(gttBaseUrl + LOCATION_V1_LOCATION)
                .build().encode().toUriString();
    }

    private String buildLocationServiceWithAltKeyParam(final String altKeyValue) {
        return UriComponentsBuilder.fromHttpUrl(buildLocationServiceUrl())
                .queryParam(FORMAT, JSON)
                .queryParam(INLINECOUNT, ALL_PAGES)
                .queryParam(FILTER, format("%s eq '%s'", LOCATION_ALT_KEY_PARAM, altKeyValue))
                .build().encode().toUriString();
    }

    private String buildLocationServiceUrlByUri(final String uri) {
        return UriComponentsBuilder
                .fromHttpUrl(gttBaseUrl + LOCATION_V1 + uri)
                .build().toUriString();
    }

    private String buildEventTypesOfTpServiceUrl(final String trackedProcess) {
        String processType = format("%s.%s", trackedProcess, trackedProcess);
        return UriComponentsBuilder
                .fromHttpUrl(buildMetadataServiceUrl())
                .pathSegment(COMBINED_MODEL_PATH_SEGMENT, processType, ADMISSIBLE_EVENT_TYPES)
                .build().encode().toUriString();
    }
}
