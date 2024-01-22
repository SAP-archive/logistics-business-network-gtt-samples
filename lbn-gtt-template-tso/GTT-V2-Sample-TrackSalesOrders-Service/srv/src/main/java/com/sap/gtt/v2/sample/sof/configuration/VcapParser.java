package com.sap.gtt.v2.sample.sof.configuration;

import com.google.gson.Gson;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.sap.cloud.security.xsuaa.mtls.SSLContextFactory;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.http.*;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.Map;

import static com.sap.gtt.v2.sample.sof.exception.SOFServiceException.MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND;


@Component
@Profile({"cloud", "local"})
public class VcapParser {
    private final Logger logger = LoggerFactory.getLogger(VcapParser.class);
    public static final String GRANT_TYPE = "grant_type";
    public static final String CLIENT_CREDENTIALS = "client_credentials";
    public static final String RESPONSE_TYPE = "response_type";
    public static final String TOKEN = "token";
    public static final String SUB_ACCOUNT_DEST_PATH = "/destination-configuration/v1/subaccountDestinations/";
    public static final String DESTINATION = "destination";
    public static final String CREDENTIALS = "credentials";
    public static final String URL = "url";
    public static final String CLIENT_ID = "clientid";
    public static final String CLIENT_UNDERLINE_ID = "client_id";
    public static final String CLIENT_SECRET = "clientsecret";
    public static final String CERTIFICATE = "certificate";
    public static final String CREDENTIAL_TYPE = "credential-type";
    public static final String CREDENTIAL_TYPE_X509 = "x509";
    public static final String KEY = "key";
    public static final String CERT_URL = "certurl";
    public static final String URI = "uri";
    public static final String ACCESS_TOKEN = "access_token";

    @Value("${VCAP_SERVICES:#{null}}")
    private String vcapServices;

    @Autowired
    private RestTemplate restTemplate;

    public Destination getDestination(String destinationName) {
        try {
            JsonObject jsonObject = new Gson().fromJson(vcapServices, JsonObject.class);
            JsonObject credentials = jsonObject.getAsJsonArray(DESTINATION).get(0)
                    .getAsJsonObject().getAsJsonObject(CREDENTIALS);

            String jwt = requestTechniqueToken(credentials);


            HttpHeaders headers = new HttpHeaders();
            headers.setBearerAuth(jwt);

            HttpEntity<String> httpEntity = new HttpEntity<>(null, headers);
            String destinationServiceUri = credentials.get(URI).getAsString() + SUB_ACCOUNT_DEST_PATH + destinationName;

            ResponseEntity<String> response = restTemplate.exchange(destinationServiceUri, HttpMethod.GET, httpEntity, String.class);
            return new Gson().fromJson(response.getBody(), Destination.class);

        } catch (JsonParseException | HttpClientErrorException e) {
            logger.error("getDestination failed.", e);
            throw new SOFServiceException(MESSAGE_CODE_DESTINATION_SERVICE_BINDING_NOT_FOUND);
        }
    }

    public String requestTechniqueToken(JsonObject credentials) {
        String token = null;
        String credentialType = credentials.has(CREDENTIAL_TYPE) ? credentials.get(CREDENTIAL_TYPE).getAsString() : null;
        String certificate = credentials.has(CERTIFICATE) ? credentials.get(CERTIFICATE).getAsString() : null;
        String key = credentials.has(KEY) ? credentials.get(KEY).getAsString() : null;
        String clientId = credentials.has(CLIENT_ID) ? credentials.get(CLIENT_ID).getAsString() : null;
        String clientSecret = credentials.has(CLIENT_SECRET) ? credentials.get(CLIENT_SECRET).getAsString() : null;
        String certUrl = credentials.has(CERT_URL) ? credentials.get(CERT_URL).getAsString() : null;
        String authUrl = credentials.has(URL) ? credentials.get(URL).getAsString() : null;

        if (StringUtils.equals(credentialType, CREDENTIAL_TYPE_X509)) {
            token = requestCertificateToken(certUrl, certificate, key, clientId);
        } else {
            token = requestClientSecretToken(authUrl, clientId, clientSecret);
        }
        return token;
    }

    public String requestClientSecretToken(String uaaUrl, String clientId, String secret) {
        String endpoint = uaaUrl + "/oauth/token?grant_type=client_credentials";
        HttpHeaders headers = new HttpHeaders();
        headers.setBasicAuth(clientId, secret);

        HttpEntity<String> httpEntity = new HttpEntity<>(null, headers);
        ResponseEntity<String> tokenResponse = restTemplate.exchange(endpoint, HttpMethod.GET, httpEntity, String.class);
        JsonObject tokenJsonObject = new Gson().fromJson(tokenResponse.getBody(), JsonObject.class);
        return tokenJsonObject.get(ACCESS_TOKEN).getAsString();
    }

    public String requestCertificateToken(String certUrl, String certificate, String key, String clientId) {
        SSLContext sslContext;
        CloseableHttpClient client = null;
        try {
            sslContext = SSLContextFactory.getInstance().create(certificate, key);

            SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(sslContext);
            client = HttpClients.custom()
                    .setSSLContext(sslContext)
                    .setSSLSocketFactory(socketFactory)
                    .build();
            HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory(client);
            RestTemplate uaaRestTemplate = new RestTemplate(factory);

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
            MultiValueMap<String, String> map= new LinkedMultiValueMap<>();
            map.add(GRANT_TYPE, CLIENT_CREDENTIALS);
            map.add(CLIENT_UNDERLINE_ID, clientId);
            map.add(RESPONSE_TYPE, TOKEN);
            HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(map, headers);

            String url = certUrl + "/oauth/token";
            @SuppressWarnings("rawtypes")
            ResponseEntity<Map> responseEntity = uaaRestTemplate.postForEntity(url, request, Map.class);
            return (String) responseEntity.getBody().get(ACCESS_TOKEN);
        } catch (IOException | GeneralSecurityException e) {
            throw new HttpClientErrorException(HttpStatus.BAD_REQUEST);
        }
    }
}
