package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class SalesOrderODataHandler extends SOFDefaultODataHandler {

    public static final String SOF_DEFAULT_O_DATA_HANDLER = "SOFDefaultODataHandler";

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        ODataResultList<SalesOrder> entityList = gttCoreServiceClient.readEntitySet(uri, SalesOrder.class);
        return convertResults(entityList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        SalesOrder salesOrder = gttCoreServiceClient.readEntity(uri, SalesOrder.class);
        return ODataUtils.toMap(salesOrder);
    }

}
