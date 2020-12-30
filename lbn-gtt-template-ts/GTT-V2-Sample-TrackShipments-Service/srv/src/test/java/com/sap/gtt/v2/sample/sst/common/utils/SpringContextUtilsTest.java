package com.sap.gtt.v2.sample.sst.common.utils;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;

@ExtendWith(MockitoExtension.class)
class SpringContextUtilsTest {

    @Mock
    private ApplicationContext applicationContext;
    @InjectMocks
    private SpringContextUtils springContextUtils;

    @Test
    void setApplicationContext_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> springContextUtils.setApplicationContext(applicationContext));
    }

    @Test
    void isSpringEnable_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(SpringContextUtils::isSpringEnable);
    }

    @Test
    void getApplicationContext_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(SpringContextUtils::getApplicationContext);
    }

    @Test
    void getBean__givenBeanName_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> SpringContextUtils.getBean("test"));
    }

    @Test
    void getBean_givenBeanClass_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> SpringContextUtils.getBean(Class.class));
    }

    @Test
    void getBean_givenBeanNameAndClass_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(() -> SpringContextUtils.getBean("test", Class.class));
    }

    @Test
    void getMessageSource_shouldNotThrowException() {
        // when-then
        assertDoesNotThrow(SpringContextUtils::getMessageSource);
    }
}
