package com.sap.gtt.v2.sample.sof.odata.bootstrap;

import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.commons.HttpStatusCodes;
import org.apache.olingo.odata2.api.processor.ODataErrorContext;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.powermock.modules.junit4.PowerMockRunner;

import javax.annotation.Resource;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

@RunWith(PowerMockRunner.class)
public class SOFODataServiceFactoryTest {

    @InjectMocks
    @Resource
    private SOFODataServiceFactory factory;


    @Test
    public void testGetCallback() {

        ODataCallback res = factory.getCallback(SOFODataServiceFactory.GlobalErrorCallback.class);

        assertNotNull(res);
        assertTrue(res instanceof SOFODataServiceFactory.GlobalErrorCallback);
    }

    @Test
    public void testHandleError() {
        ODataErrorContext context = new ODataErrorContext();
        context.setContentType("application/json");
        context.setHttpStatus(HttpStatusCodes.INTERNAL_SERVER_ERROR);

        SOFODataServiceFactory.GlobalErrorCallback errorCallback = new SOFODataServiceFactory.GlobalErrorCallback();
        ODataResponse res = errorCallback.handleError(context);
        assertNotNull(res);
    }

}
