package com.sap.gtt.v2.sample.sof.odata.bootstrap;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.Collection;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class BootstrapServletRegistrationBeanTest {

    @Test
    public void testBootstrapServletRegistrationBean() {
        BootstrapServlet servlet = mock(BootstrapServlet.class);
        BootstrapServletRegistrationBean bean = new BootstrapServletRegistrationBean(servlet);

        Collection<String> urlMappings = bean.getUrlMappings();

        assertFalse(urlMappings.isEmpty());
        assertEquals(1, urlMappings.size());
        assertEquals("/sap/logistics/gtt/sample/sof/odata/v1/*", urlMappings.iterator().next());
    }

}