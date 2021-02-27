package com.sap.gtt.v2.sample.sof.odata.handler;


import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrderItem;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @Author: Sarah
 * @Date: 5/24/2020 2:25 PM
 */
@Component
public class SOIOdataHandler extends SOFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(SOIOdataHandler.class);
    @Autowired
    private SOFService sofService;
    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo,
                                                                    ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        ODataResultList<SalesOrderItem> entityList = gttCoreServiceClient.readEntitySet(uri, SalesOrderItem.class);
        return convertResults(entityList);
    }


    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        SalesOrderItem salesOrderItem = gttCoreServiceClient.readEntity(uri, SalesOrderItem.class);
        return ODataUtils.toMap(salesOrderItem);
    }


}
