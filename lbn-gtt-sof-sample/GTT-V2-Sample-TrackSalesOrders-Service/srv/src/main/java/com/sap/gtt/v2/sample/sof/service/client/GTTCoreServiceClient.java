package com.sap.gtt.v2.sample.sof.service.client;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.Location;
import com.sap.gtt.v2.sample.sof.exception.GeneralNoneTranslatableException;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterExpressionBuilder;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.expression.FilterExpression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import static com.sap.gtt.v2.sample.sof.exception.SOFServiceException.*;

@Service
public class GTTCoreServiceClient {
    public static final String OUTBOUND_ODATA_V_1 = "/outbound/odata/v1/";
    public static final String INBOUND_REST_V_1 = "/inbound/rest/v1/";
    public static final String METADATA_V_1_MODEL = "/metadata/v1/model/";
    public static final String METADATA_V_1_MODEL1 = "/metadata/v1/model/";

    private static final Logger logger = LoggerFactory.getLogger(GTTCoreServiceClient.class);
    public static final String LOCATION_V_1_LOCATION = "/location/v1/Location";
    public static final String FORMAT = "$format";
    public static final String JSON = "json";
    public static final String INLINECOUNT = "$inlinecount";
    public static final String ALLPAGES = "allpages";
    public static final String FILTER = "$filter";

    @Autowired
    private RestTemplate restTemplate;

    @Value("${GTT_CORE_ENGINE_API_URL_FLP_BASED}")
    private String gttBaseUrl;

    @Value("${GTT_CORE_ENGINE_TECHNICAL_USER}")
    private String techUser;

    @Value("${GTT_CORE_ENGINE_TECHNICAL_PWD}")
    private String criticalInfo;

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
        if (headers != null) {
            finalHeaders.addAll(headers);
        }
        finalHeaders.setBasicAuth(techUser, criticalInfo);
        if (finalHeaders.getAcceptLanguage() == null || finalHeaders.getAcceptLanguage().isEmpty()) {
            finalHeaders.setAcceptLanguageAsLocales(Collections.singletonList(LocaleContextHolder.getLocale()));
        }

        String readServiceUrl = UriComponentsBuilder.fromHttpUrl(gttBaseUrl + OUTBOUND_ODATA_V_1 +
                Constants.MODEL_NAMESPACE + uri).build().encode().toUriString();
        logger.debug("ready to call read service: {}", readServiceUrl);

        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(readServiceUrl, HttpMethod.GET,
                    new HttpEntity<>(null, finalHeaders), String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call read service failed:", e);
            throw new SOFServiceException(MESSAGE_CODE_CALL_READ_SERVICE_FAILED);
        }
        return responseEntity.getBody();

    }

    public void write(String body, String uri) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(techUser, criticalInfo);

        String writeServiceUrl = gttBaseUrl + INBOUND_REST_V_1 + Constants.WRITE_SERVICE_MODEL_NAMESPACE + uri;
        logger.debug("ready to call write service: {}", writeServiceUrl);
        try {
            restTemplate.exchange(writeServiceUrl, HttpMethod.POST, new HttpEntity<>(body, headers), String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call write service failed:", e);
            throw new SOFServiceException(MESSAGE_CODE_CALL_WRITE_SERVICE_FAILED);
        }
    }

    public String getUiAnnotation() {
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(techUser, criticalInfo);

        String metadataServiceUrl = gttBaseUrl + METADATA_V_1_MODEL + Constants.GTT_MODEL_NAMESPACE + "/annotation";
        logger.debug("ready to call metadata service: {}", metadataServiceUrl);
        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(metadataServiceUrl, HttpMethod.GET,
                    new HttpEntity<>(null, headers), String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call metadata service failed:", e);
            throw new SOFServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }
        return responseEntity.getBody();
    }

    public String getI18n(String properties) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(techUser, criticalInfo);

        String metadataServiceUrl = gttBaseUrl + METADATA_V_1_MODEL1 + Constants.GTT_MODEL_NAMESPACE + "/i18n/" + properties;
        logger.debug("ready to call metadata service: {}", metadataServiceUrl);
        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(metadataServiceUrl, HttpMethod.GET,
                    new HttpEntity<>(null, headers), String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call metadata service failed:", e);
            throw new SOFServiceException(MESSAGE_CODE_CALL_METADATA_SERVICE_FAILED);
        }

        return responseEntity.getBody();
    }

    public Location getLocation(String locationAltKey) {
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(techUser, criticalInfo);

        // encode().build() has unexpected result, use build().encode()
        String locationService = UriComponentsBuilder.fromHttpUrl(gttBaseUrl + LOCATION_V_1_LOCATION)
                .queryParam(FORMAT, JSON)
                .queryParam(INLINECOUNT, ALLPAGES)
                .queryParam(FILTER, "locationAltKey eq '" + locationAltKey + "'").build().encode().toUriString();

        logger.debug("ready to call location service: {}", locationService);
        ResponseEntity<String> responseEntity;
        try {
            responseEntity = restTemplate.exchange(locationService, HttpMethod.GET,
                    new HttpEntity<>(null, headers), String.class);
        } catch (HttpStatusCodeException e) {
            logger.error("Call location service failed:", e);
            throw new SOFServiceException(MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED);
        }

        ODataResultList<Location> res = ODataUtils.readEntitySet(responseEntity.getBody(), Location.class);

        return !res.getResults().isEmpty() ? res.getResults().get(0) : null;
    }

    public List<Location> getLocations(Set<String> locationAltKeys) {
        List<Location> result = new ArrayList<>();

        if (!locationAltKeys.isEmpty()) {
            HttpHeaders headers = new HttpHeaders();
            headers.setBasicAuth(techUser, criticalInfo);
            List<FilterCondition> conditions = new ArrayList<>();
            locationAltKeys.forEach(locationAltKey ->
                    conditions.add(new FilterCondition("locationAltKey", FilterCondition.EDM_TYPE_STRING, locationAltKey, BinaryOperator.EQ)));
            FilterExpression filter = FilterExpressionBuilder.createFilterExpression(conditions, BinaryOperator.OR);
            assert filter != null;
            String locationService = UriComponentsBuilder.fromHttpUrl(gttBaseUrl + LOCATION_V_1_LOCATION)
                    .queryParam(FORMAT, JSON)
                    .queryParam(INLINECOUNT, ALLPAGES)
                    .queryParam(FILTER, filter.getExpressionString()).build().encode().toUriString();

            ResponseEntity<String> responseEntity;
            try {
                responseEntity = restTemplate.exchange(locationService, HttpMethod.GET,
                        new HttpEntity<>(null, headers), String.class);
            } catch (HttpStatusCodeException e) {
                logger.error("Call location service failed:", e);
                throw new SOFServiceException(MESSAGE_CODE_CALL_LOCATION_SERVICE_FAILED);
            }

            result = ODataUtils.readEntitySet(responseEntity.getBody(), Location.class).getResults();
        }

        return result;
    }
}
