package com.sap.gtt.v2.sample.sst.common.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;

/**
 * {@link MessageSourceConfig} is a config for source messages.
 *
 * @author Min Li
 */
@Configuration
public class MessageSourceConfig {

    @Bean
    public ReloadableResourceBundleMessageSource messageSource() {
        final ReloadableResourceBundleMessageSource messageSource = new ReloadableResourceBundleMessageSource();
        messageSource.setDefaultEncoding("UTF-8");
        messageSource.setFallbackToSystemLocale(false);
        messageSource.setUseCodeAsDefaultMessage(false);
        messageSource.setBasenames("classpath:i18n/messages");  // name of the resource bundle
        return messageSource;
    }
}
