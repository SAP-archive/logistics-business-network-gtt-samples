package com.sap.gtt.v2.sample.sst.odata.bootstrap;

import java.io.IOException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.core.servlet.ODataServlet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * {@link BootstrapServlet} is HTTP OData servlet.
 *
 * @author Min Li
 */
@Component
public class BootstrapServlet extends ODataServlet {

    @Autowired
    private SSTODataServiceFactory factory;

    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        req.setAttribute(ODataServiceFactory.FACTORY_INSTANCE_LABEL, factory);
        super.service(req, resp);
    }
}
