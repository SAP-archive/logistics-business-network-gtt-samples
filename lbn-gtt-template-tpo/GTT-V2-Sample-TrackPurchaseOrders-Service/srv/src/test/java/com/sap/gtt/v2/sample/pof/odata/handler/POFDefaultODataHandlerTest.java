package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.io.IOUtils;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.edm.EdmTypeKind;
import org.apache.olingo.odata2.core.uri.UriInfoImpl;
import org.assertj.core.api.Assertions;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.springframework.core.io.ClassPathResource;

import java.io.IOException;
import java.util.Map;

import static com.sap.gtt.v2.sample.pof.constant.Constants.MODEL_NAMESPACE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mockStatic;

@RunWith(PowerMockRunner.class)
@PrepareForTest(POFUtils.class)
public class POFDefaultODataHandlerTest {

    private static final String QUERY_PURCHASE_ORDER_ITEM = "sap/logistics/gtt/sample/pof/odata/v1/" + PurchaseOrderItem.ENTITY_SET_NAME;

    @Mock
    private GTTCoreServiceClient client;

    @InjectMocks
    private POFDefaultODataHandler handler;

    @Before
    public void setUp() {
        mockStatic(POFUtils.class);
        given(POFUtils.getGson()).willCallRealMethod();
        given(POFUtils.getTargetName(any())).willCallRealMethod();
    }

    @Test
    public void queryPurchaseOrderItemSet() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);
        when(this.client.readEntitySet(anyString(), eq(PurchaseOrderItem.class))).thenReturn(entityList);
        given(POFUtils.getNormalizedUri(any())).willReturn(QUERY_PURCHASE_ORDER_ITEM);

        UriInfoImpl uriInfo = new UriInfoImpl();
        EdmTypeTestImpl targetType = new EdmTypeTestImpl();
        targetType.setName(PurchaseOrderItem.ENTITY_SET_NAME);
        uriInfo.setTargetType(targetType);

        ODataResultList<Map<String, Object>> response = this.handler.handleReadEntitySet(uriInfo, null);
        Assert.assertEquals(2, response.getResults().size());
    }

    @Test
    public void queryPurchaseOrderItem() throws IOException {
        String json = IOUtils.toString(new ClassPathResource("/odata/purchase-order-item.json").getInputStream());
        ODataResultList<PurchaseOrderItem> entityList = ODataUtils.readEntitySet(json, PurchaseOrderItem.class);
        when(this.client.readEntity(anyString(), eq(PurchaseOrderItem.class))).thenReturn(entityList.getResults().get(0));
        given(POFUtils.getNormalizedUri(any())).willReturn(QUERY_PURCHASE_ORDER_ITEM);

        UriInfoImpl uriInfo = new UriInfoImpl();
        EdmTypeTestImpl targetType = new EdmTypeTestImpl();
        targetType.setName(PurchaseOrderItem.ENTITY_SET_NAME);
        uriInfo.setTargetType(targetType);

        Map<String, Object> response = this.handler.handleReadEntity(uriInfo, null);
        Assertions.assertThat(response).isNotEmpty();
    }

    @Test(expected = POFServiceException.class)
    public void queryDummyEntity() {
        UriInfoImpl uriInfo = new UriInfoImpl();
        EdmTypeTestImpl targetType = new EdmTypeTestImpl();
        targetType.setName("Dummy");
        uriInfo.setTargetType(targetType);

        this.handler.handleReadEntity(uriInfo, null);
    }


    private static class EdmTypeTestImpl implements EdmType {

        private String name;

        @Override
        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        @Override
        public String getNamespace() {
            return MODEL_NAMESPACE;
        }

        @Override
        public EdmTypeKind getKind() {
            return EdmTypeKind.COMPLEX;
        }
    }
}
