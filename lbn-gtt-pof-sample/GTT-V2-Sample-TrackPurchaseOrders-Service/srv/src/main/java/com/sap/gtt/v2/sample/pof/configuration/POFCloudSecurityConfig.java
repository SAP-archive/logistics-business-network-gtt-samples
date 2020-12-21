package com.sap.gtt.v2.sample.pof.configuration;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;

import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;


@Configuration
@EnableWebSecurity
@Profile({"cloud"})
public class POFCloudSecurityConfig extends AbstractCloudSecurityConfiguration {
	private static final Logger logger = LoggerFactory.getLogger(POFCloudSecurityConfig.class);
	
	private static final String PURCHASE_ORDER_TRACKING_DISPLAY = "pof.r";

	@Override
	public ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry setProtectedExpressionInterceptUrlRegistry(
			ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry) {

		ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry registry = super.setProtectedExpressionInterceptUrlRegistry(expressionInterceptUrlRegistry);

		registry.antMatchers(GET, "/sap/logistics/gtt/sample/pof/odata/v1/**").access(getScopeCheckExpress(PURCHASE_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/pof/odata/v1/**").access(getScopeCheckExpress(PURCHASE_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(GET, "/sap/logistics/gtt/sample/pof/rest/v1/**").access(getScopeCheckExpress(PURCHASE_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/pof/rest/v1/**").access(getScopeCheckExpress(PURCHASE_ORDER_TRACKING_DISPLAY));

		return registry;
	}
    
}
