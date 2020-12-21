package com.sap.gtt.v2.sample.pof.configuration.local;

import com.sap.gtt.v2.sample.pof.configuration.AbstractCloudSecurityConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;

import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.POST;


@Configuration
@EnableWebSecurity
@Profile({ "local", "test" })
public class LocalSecurityConfig extends AbstractCloudSecurityConfiguration {

	@Override
	public void configure(HttpSecurity http) throws Exception {
		http.csrf().disable();
		super.configure(http);
	}

	@Override
	public ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry setProtectedExpressionInterceptUrlRegistry(
			ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry) {

		ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry registry = super.setProtectedExpressionInterceptUrlRegistry(expressionInterceptUrlRegistry);

		registry.antMatchers(GET, "/sap/logistics/gtt/sample/pof/odata/v1/**").permitAll();
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/pof/odata/v1/**").permitAll();
		registry.antMatchers(GET, "/sap/logistics/gtt/sample/pof/rest/v1/**").permitAll();
		registry.antMatchers(POST, "/sap/logistics/gtt/sample/pof/rest/v1/**").permitAll();

		return registry;
	}

}
