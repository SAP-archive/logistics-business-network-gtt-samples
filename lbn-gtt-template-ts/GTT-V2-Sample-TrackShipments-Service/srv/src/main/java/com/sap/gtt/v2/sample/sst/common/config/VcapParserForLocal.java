package com.sap.gtt.v2.sample.sst.common.config;

import com.sap.gtt.v2.sample.sst.common.model.Destination;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

/**
 * {@link VcapParserForLocal} is a parser of VCAP variables for local and test environment.
 *
 * @author Min Li
 */
@Component
@Profile({"local", "test"})
public class VcapParserForLocal extends VcapParser {

    @Value("${GTT_CORE_ENGINE_API_URL_FLP_BASED}")
    private String gttBaseUrl;

    @Value("${GTT_CORE_ENGINE_TECHNICAL_USER}")
    private String techUser;

    @Value("${GTT_CORE_ENGINE_TECHNICAL_PWD}")
    private String criticalInfo;

    @Override
    public Destination getDestination(String destinationName) {
        Destination destination = new Destination();
        destination.setUrl(gttBaseUrl);
        destination.setUser(techUser);
        destination.setPassword(criticalInfo);
        return destination;
    }
}
