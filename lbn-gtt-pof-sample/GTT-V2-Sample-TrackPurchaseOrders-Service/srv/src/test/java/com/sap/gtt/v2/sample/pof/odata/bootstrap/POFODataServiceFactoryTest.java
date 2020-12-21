package com.sap.gtt.v2.sample.pof.odata.bootstrap;

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
public class POFODataServiceFactoryTest {

    @InjectMocks
    @Resource
    private POFODataServiceFactory factory;


    @Test
    public void testGetCallback() {

        ODataCallback res = factory.getCallback(POFODataServiceFactory.GlobalErrorCallback.class);

        assertNotNull(res);
        assertTrue(res instanceof POFODataServiceFactory.GlobalErrorCallback);
    }

    @Test
    public void testHandleError() {
        ODataErrorContext context = new ODataErrorContext();
        context.setContentType("application/json");
        context.setHttpStatus(HttpStatusCodes.INTERNAL_SERVER_ERROR);

        POFODataServiceFactory.GlobalErrorCallback errorCallback = new POFODataServiceFactory.GlobalErrorCallback();
        ODataResponse res = errorCallback.handleError(context);
        assertNotNull(res);
    }

}
