package com.sap.gtt.v2.sample.sst.common.config;

import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;

/**
 * {@link SSTCloudSecurityConfig} is a security config for cloud environment.
 *
 * @author Min Li
 */
@Configuration
@EnableWebSecurity
@Profile({"cloud"})
public class SSTCloudSecurityConfig extends AbstractCloudSecurityConfig {

    private static final String SHIPMENT_TRACKING_DISPLAY = "sst.r";

    @Override
    public ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry setProtectedExpressionInterceptUrlRegistry(
            ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry) {

        ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry registry = super.setProtectedExpressionInterceptUrlRegistry(expressionInterceptUrlRegistry);

        registry.antMatchers(GET, "/sap/logistics/gtt/sample/sst/odata/v1/**").access(getScopeCheckExpress(SHIPMENT_TRACKING_DISPLAY));
        registry.antMatchers(POST, "/sap/logistics/gtt/sample/sst/odata/v1/**").access(getScopeCheckExpress(SHIPMENT_TRACKING_DISPLAY));
        registry.antMatchers(GET, "/sap/logistics/gtt/sample/sst/rest/v1/**").access(getScopeCheckExpress(SHIPMENT_TRACKING_DISPLAY));
        registry.antMatchers(POST, "/sap/logistics/gtt/sample/sst/rest/v1/**").access(getScopeCheckExpress(SHIPMENT_TRACKING_DISPLAY));

        return registry;
    }
}
