package com.sap.gtt.v2.sample.pof.odata.bootstrap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.stereotype.Component;

@Component
public class BootstrapServletRegistrationBean extends ServletRegistrationBean {
    public static final String ROOT_URL = "/sap/logistics/gtt/sample/pof/odata/v1";
    @Autowired
    public BootstrapServletRegistrationBean(
            BootstrapServlet servlet) {
        super(servlet, ROOT_URL + "/*");
    }
}
