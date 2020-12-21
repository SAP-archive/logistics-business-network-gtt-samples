package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetCountUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;

import java.util.Map;

public interface POFODataHandler {

    void register(String targetType, Class<?> clazz);

    ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext);

    Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext);

    Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext);

}
