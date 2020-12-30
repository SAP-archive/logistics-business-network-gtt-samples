package com.sap.gtt.v2.sample.pof.configuration;

import com.sap.cloud.security.xsuaa.XsuaaServiceConfiguration;
import com.sap.cloud.security.xsuaa.token.TokenAuthenticationConverter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.security.access.AccessDecisionVoter;
import org.springframework.security.access.expression.AbstractSecurityExpressionHandler;
import org.springframework.security.access.vote.AuthenticatedVoter;
import org.springframework.security.access.vote.UnanimousBased;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;
import org.springframework.security.config.annotation.web.configurers.oauth2.server.resource.OAuth2ResourceServerConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.access.expression.DefaultWebSecurityExpressionHandler;
import org.springframework.security.web.access.expression.WebExpressionVoter;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractCloudSecurityConfiguration extends WebSecurityConfigurerAdapter {

    @Autowired
    private XsuaaServiceConfiguration xsuaaServiceConfiguration;

    @Autowired
    private AbstractSecurityExpressionHandler expressionHandler;

    @Bean(name = "accessDecisionManager")
    protected UnanimousBased accessDecisionManagerBean() {
        List<AccessDecisionVoter<?>> voterList = new ArrayList<>();
        WebExpressionVoter expressionVoter = new WebExpressionVoter();
        expressionVoter.setExpressionHandler(expressionHandler);
        voterList.add(expressionVoter);
        voterList.add(new AuthenticatedVoter());
        return new UnanimousBased(voterList);
    }

    /**
     * Customizes how GrantedAuthority are derived from a Jwt.
     *
     * @return jwt converter
     */
    private TokenAuthenticationConverter getJwtAuthoritiesConverter() {
        TokenAuthenticationConverter converter = new TokenAuthenticationConverter(xsuaaServiceConfiguration);
        converter.setLocalScopeAsAuthorities(true);
        return converter;
    }



    /**
     * This is to fix a bug in the original DefaultWebSecurityExpressionHandler
     * which no bean resolver set so that you can not use bean in the scope check SPEL
     *
     * @param applicationContext
     * @return
     */
    @Bean
    public AbstractSecurityExpressionHandler expressionHandler(ApplicationContext applicationContext) {
        AbstractSecurityExpressionHandler webExpressionHandler = new DefaultWebSecurityExpressionHandler();
        webExpressionHandler.setApplicationContext(applicationContext);
        return webExpressionHandler;
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        http.headers().contentSecurityPolicy("script-src 'self'");

        ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry = http.sessionManagement()
                .sessionCreationPolicy(SessionCreationPolicy.NEVER)
                .and().authorizeRequests();
        http.authorizeRequests()
                .accessDecisionManager(accessDecisionManagerBean());
        expressionInterceptUrlRegistry = this.setProtectedExpressionInterceptUrlRegistry(expressionInterceptUrlRegistry);
        OAuth2ResourceServerConfigurer<HttpSecurity> oauth2ResourceServer = expressionInterceptUrlRegistry
                .and().oauth2ResourceServer();

        oauth2ResourceServer
                .jwt()
                .jwtAuthenticationConverter(getJwtAuthoritiesConverter());

    }

    public ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry setProtectedExpressionInterceptUrlRegistry(ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry expressionInterceptUrlRegistry) {
        return expressionInterceptUrlRegistry.antMatchers("/lbnServiceHealth**").anonymous();
    }

    protected String getScopeCheckExpress(String scope) {
        return this.getScopeCheckExpress(scope, false);
    }

    protected String getScopeCheckExpress(String scope, boolean masterUaa) {
        String result = "hasAuthority('" + scope + "')";
        if (masterUaa) {
            result = result + " and !@currentAccessContext.containsCloneServiceInstanceId()";
        }

        return result;
    }
}
