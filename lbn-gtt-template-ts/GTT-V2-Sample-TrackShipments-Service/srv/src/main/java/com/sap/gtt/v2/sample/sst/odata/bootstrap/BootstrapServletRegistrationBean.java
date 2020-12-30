package com.sap.gtt.v2.sample.sst.odata.bootstrap;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.ODATA_ROOT_URL;

import javax.servlet.Servlet;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.stereotype.Component;

/**
 * {@link BootstrapServletRegistrationBean} registers servlet with root URL.
 *
 * @author Min Li
 */
@Component
public class BootstrapServletRegistrationBean extends ServletRegistrationBean<Servlet> {

    public BootstrapServletRegistrationBean(final BootstrapServlet servlet) {
        super(servlet, ODATA_ROOT_URL + "/*");
    }
}
