package com.sap.gtt.v2.sample.pof.odata.bootstrap;

import org.apache.olingo.odata2.api.ODataServiceFactory;
import org.apache.olingo.odata2.core.servlet.ODataServlet;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;


@Component
public class BootstrapServlet extends ODataServlet {

    private final ODataServiceFactory factory;

    public BootstrapServlet(ODataServiceFactory factory) {
        this.factory = factory;
    }

    @Override
    protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        req.setAttribute(ODataServiceFactory.FACTORY_INSTANCE_LABEL, factory);
        super.service(req, resp);
    }
}
