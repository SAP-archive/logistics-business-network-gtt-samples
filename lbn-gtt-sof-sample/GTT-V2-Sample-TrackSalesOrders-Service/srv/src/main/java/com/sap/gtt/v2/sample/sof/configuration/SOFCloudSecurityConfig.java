package com.sap.gtt.v2.sample.sof.configuration;

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
public class SOFCloudSecurityConfig extends AbstractCloudSecurityConfiguration {
	private static final Logger logger = LoggerFactory.getLogger(SOFCloudSecurityConfig.class);
	
	private static final String SALES_ORDER_TRACKING_DISPLAY = "sof.r";

	@Override
	public ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry setProtectedExpressionInterceptUrlRegistry(
			ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry) {

		ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry registry = super.setProtectedExpressionInterceptUrlRegistry(expressionInterceptUrlRegistry);

		registry.antMatchers(GET, "/sap/logistics/gtt/sample/sof/odata/v1/**").access(getScopeCheckExpress(SALES_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/sof/odata/v1/**").access(getScopeCheckExpress(SALES_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(GET, "/sap/logistics/gtt/sample/sof/rest/v1/**").access(getScopeCheckExpress(SALES_ORDER_TRACKING_DISPLAY));
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/sof/rest/v1/**").access(getScopeCheckExpress(SALES_ORDER_TRACKING_DISPLAY));

		return registry;
	}
    
}
