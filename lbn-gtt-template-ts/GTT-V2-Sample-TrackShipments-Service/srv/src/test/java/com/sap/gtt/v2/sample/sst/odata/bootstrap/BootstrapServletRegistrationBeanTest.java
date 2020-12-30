package com.sap.gtt.v2.sample.sst.odata.bootstrap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.mock;

import java.util.Collection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class BootstrapServletRegistrationBeanTest {

    @Test
    void testBootstrapServletRegistrationBean() {
        BootstrapServlet servlet = mock(BootstrapServlet.class);
        BootstrapServletRegistrationBean bean = new BootstrapServletRegistrationBean(servlet);

        Collection<String> urlMappings = bean.getUrlMappings();

        assertFalse(urlMappings.isEmpty());
        assertEquals(1, urlMappings.size());
        assertEquals("/sap/logistics/gtt/sample/sst/odata/v1/*", urlMappings.iterator().next());
    }
}
