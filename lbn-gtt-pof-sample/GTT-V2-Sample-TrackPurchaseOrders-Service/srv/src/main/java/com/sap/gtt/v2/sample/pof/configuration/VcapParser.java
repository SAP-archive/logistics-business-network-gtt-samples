package com.sap.gtt.v2.sample.pof.configuration;

import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import static com.sap.gtt.v2.sample.pof.exception.POFServiceException.MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND;
import static com.sap.gtt.v2.sample.pof.utils.POFUtils.getGson;


@Component
@Profile({"cloud", "local"})
public class VcapParser {
    private Logger logger = LoggerFactory.getLogger(VcapParser.class);

    public static final String OAUTH_TOKEN_CLIENT_CREDENTIALS_PATH = "/oauth/token?grant_type=client_credentials";
    private static final String SUBACCOUNT_DEST_PATH = "/destination-configuration/v1/subaccountDestinations/";
    public static final String DESTINATION = "destination";
    public static final String CREDENTIALS = "credentials";
    public static final String URL = "url";
    public static final String CLIENTID = "clientid";
    public static final String CLIENTSECRET = "clientsecret";
    public static final String URI = "uri";
    public static final String ACCESS_TOKEN = "access_token";

    @Value("${VCAP_SERVICES:#{null}}")
    private String vcapServices;

    @Autowired
    private RestTemplate restTemplate;

    public Destination getDestination(String destinationName) {
        try {
            JsonObject jsonObject = getGson().fromJson(vcapServices, JsonObject.class);
            JsonObject credentials = jsonObject.getAsJsonArray(DESTINATION).get(0)
                    .getAsJsonObject().getAsJsonObject(CREDENTIALS);

            String authUrl = credentials.get(URL).getAsString();
            String clientId = credentials.get(CLIENTID).getAsString();
            String clientSecret = credentials.get(CLIENTSECRET).getAsString();
            String jwt = requestTechniqueToken(authUrl, clientId, clientSecret);

            HttpHeaders headers = new HttpHeaders();
            headers.setBearerAuth(jwt);

            HttpEntity httpEntity = new HttpEntity(null, headers);
            String destinationServiceUri = credentials.get(URI).getAsString() + SUBACCOUNT_DEST_PATH + destinationName;

            ResponseEntity<String> response = restTemplate.exchange(destinationServiceUri, HttpMethod.GET, httpEntity, String.class);
            return getGson().fromJson(response.getBody(), Destination.class);

        } catch (JsonParseException | HttpClientErrorException e) {
            logger.error("getDestination failed.", e);
            throw new POFServiceException(MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND);
        }
    }

    public String requestTechniqueToken(String uaaUrl, String clientId, String secret) {
        String endpoint = uaaUrl + OAUTH_TOKEN_CLIENT_CREDENTIALS_PATH;
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(clientId, secret);

        HttpEntity httpEntity = new HttpEntity(null, headers);
        ResponseEntity<String> tokenResponse = restTemplate.exchange(endpoint, HttpMethod.GET, httpEntity, String.class);
        JsonObject tokenJsonObject = getGson().fromJson(tokenResponse.getBody(), JsonObject.class);
        return tokenJsonObject.get(ACCESS_TOKEN).getAsString();
    }
}
