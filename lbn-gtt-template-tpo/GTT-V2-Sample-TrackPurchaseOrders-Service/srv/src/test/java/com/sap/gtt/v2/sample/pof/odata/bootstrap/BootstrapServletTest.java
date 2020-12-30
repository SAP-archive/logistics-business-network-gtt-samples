package com.sap.gtt.v2.sample.pof.odata.bootstrap;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import java.io.IOException;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class BootstrapServletTest {

    @Mock
    private POFODataServiceFactory factory;

    @InjectMocks
    private BootstrapServlet servlet;


    @Test
    public void service() throws IOException {

        MockHttpServletRequest req = new MockHttpServletRequest();
        MockHttpServletResponse res = new MockHttpServletResponse();
        req.setPathInfo("/sap/logistics/gtt/sample/pof/odata/v1/SalesOrder");

        ServletConfig config = mock(ServletConfig.class);
        try {
            servlet.init(config);
            servlet.service(req, res);
        } catch (ServletException | IOException e) {
            fail(e.getMessage());
        }
    }
}