package com.sap.gtt.v2.sample.pof.utils;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.support.ReloadableResourceBundleMessageSource;
import org.springframework.stereotype.Component;


@Component
public class SpringContextUtils implements ApplicationContextAware {

    private static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        SpringContextUtils.setContext(applicationContext);
    }

    public static boolean isSpringEnable() {
        return applicationContext != null;
    }

    private static void setContext(ApplicationContext applicationContext) {
        SpringContextUtils.applicationContext = applicationContext;
    }

    public static ApplicationContext getApplicationContext() {
        return applicationContext;
    }

    public static Object getBean(String name) {
        return getApplicationContext().getBean(name);
    }

    public static <T> T getBean(Class<T> clazz) {
        return getApplicationContext().getBean(clazz);
    }

    public static <T> T getBean(String name, Class<T> clazz) {
        return getApplicationContext().getBean(name, clazz);
    }

    public static String getProfile() {
        if (getApplicationContext().getEnvironment().getActiveProfiles().length > 0) {
            return getApplicationContext().getEnvironment().getActiveProfiles()[0];
        } else {
            return getApplicationContext().getEnvironment().getDefaultProfiles()[0];
        }
    }

    public static ReloadableResourceBundleMessageSource getMessageSource() {
        return applicationContext.getBean(ReloadableResourceBundleMessageSource.class);
    }
}
