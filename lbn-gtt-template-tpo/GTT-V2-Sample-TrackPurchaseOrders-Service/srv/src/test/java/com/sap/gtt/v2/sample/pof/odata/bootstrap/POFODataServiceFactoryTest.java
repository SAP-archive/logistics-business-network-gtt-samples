package com.sap.gtt.v2.sample.pof.odata.bootstrap;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.sap.gtt.v2.sample.pof.App;
import org.apache.olingo.odata2.api.ODataCallback;
import org.apache.olingo.odata2.api.processor.ODataErrorContext;
import org.apache.olingo.odata2.api.processor.ODataResponse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.HttpServerErrorException;

@RunWith(SpringRunner.class)
@SpringBootTest(classes = App.class, webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@ActiveProfiles("test")
public class POFODataServiceFactoryTest {
/*
    @InjectMocks
    @Resource*/
    @Autowired
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
        /*context.setHttpStatus(HttpStatusCodes.INTERNAL_SERVER_ERROR);*/
        context.setException(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));
        POFODataServiceFactory.GlobalErrorCallback errorCallback = new POFODataServiceFactory.GlobalErrorCallback();
        ODataResponse res = errorCallback.handleError(context);
        assertNotNull(res);
    }

}
