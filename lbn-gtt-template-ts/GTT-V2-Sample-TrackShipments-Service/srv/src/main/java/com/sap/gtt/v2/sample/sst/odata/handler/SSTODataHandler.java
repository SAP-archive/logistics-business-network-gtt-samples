package com.sap.gtt.v2.sample.sst.odata.handler;

import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import java.util.Map;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetCountUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;

/**
 * {@link SSTODataHandler} is a common interface for handlers which process HTTP OData requests.
 *
 * @author Min Li
 */
public interface SSTODataHandler {

    /**
     * Registers provided class as OData handler.
     *
     * @param targetType - name of target type
     * @param clazz      - class of target type
     */
    void register(String targetType, Class<?> clazz);

    /**
     * Reads entity set.
     *
     * @param uriInfo      - entity which contains information about URI
     * @param oDataContext - context of OData request
     * @return result of OData request wrapped in {@link ODataResultList}
     */
    ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext);

    /**
     * Reads single entity.
     *
     * @param uriInfo      - entity which contains information about URI
     * @param oDataContext - context of OData request
     * @return result of OData request as {@link Map}
     */
    Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext);

    /**
     * Counts amount of entities in entity set.
     *
     * @param uriInfo      entity which contains information about URI
     * @param oDataContext - context of OData request
     * @return amount entities in entity set
     */
    Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext);
}
