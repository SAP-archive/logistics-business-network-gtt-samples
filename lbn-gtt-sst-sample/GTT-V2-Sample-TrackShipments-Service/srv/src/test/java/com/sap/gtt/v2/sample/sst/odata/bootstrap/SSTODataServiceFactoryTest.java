package com.sap.gtt.v2.sample.sst.odata.bootstrap;


import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.annotation.Resource;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.commons.HttpStatusCodes;
import org.apache.olingo.odata2.api.processor.ODataErrorContext;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SSTODataServiceFactoryTest {

    @Resource
    @InjectMocks
    private SSTODataServiceFactory factory;

    @Test
    void testGetCallback() {
        ODataCallback res = factory.getCallback(SSTODataServiceFactory.GlobalErrorCallback.class);

        assertNotNull(res);
        assertTrue(res instanceof SSTODataServiceFactory.GlobalErrorCallback);
    }

    @Test
    void testHandleError() {
        ODataErrorContext context = new ODataErrorContext();
        context.setContentType("application/json");
        context.setHttpStatus(HttpStatusCodes.INTERNAL_SERVER_ERROR);

        SSTODataServiceFactory.GlobalErrorCallback errorCallback = new SSTODataServiceFactory.GlobalErrorCallback();
        ODataResponse res = errorCallback.handleError(context);
        assertNotNull(res);
    }
}
