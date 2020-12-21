package com.sap.gtt.v2.sample.sst.odata.bootstrap;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import java.io.IOException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

@ExtendWith(MockitoExtension.class)
class BootstrapServletTest {

    @Mock
    private SSTODataServiceFactory factory;

    @InjectMocks
    private BootstrapServlet servlet;

    @Test
    void service() throws IOException {
        MockHttpServletRequest req = new MockHttpServletRequest();
        MockHttpServletResponse res = new MockHttpServletResponse();
        req.setPathInfo("/sap/logistics/gtt/sample/sst/odata/v1/ProcessStatus");

        ServletConfig config = mock(ServletConfig.class);
        try {
            servlet.init(config);
            servlet.service(req, res);
        } catch (ServletException | IOException e) {
            fail(e.getMessage());
        }
    }
}
