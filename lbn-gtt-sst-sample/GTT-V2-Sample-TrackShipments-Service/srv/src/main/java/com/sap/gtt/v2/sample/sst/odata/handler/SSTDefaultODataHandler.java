package com.sap.gtt.v2.sample.sst.odata.handler;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getTargetName;

import com.sap.gtt.v2.sample.sst.common.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sst.common.exception.SSTServiceException;
import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sst.odata.model.ProcessStatus;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetCountUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

/**
 * {@link SSTDefaultODataHandler} is a default handler which processes HTTP OData requests.
 *
 * @author Min Li
 */
@Primary
@Component
public class SSTDefaultODataHandler implements SSTODataHandler {

    private static final Logger logger = LoggerFactory.getLogger(SSTDefaultODataHandler.class);
    private static final Map<String, Class<?>> map = new ConcurrentHashMap<>();

    @Autowired
    protected GTTCoreServiceClient gttCoreServiceClient;

    @Override
    public void register(String targetType, Class<?> clazz) {
        map.put(targetType, clazz);
    }

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        Class<?> clazz = getClazz(uriInfo.getTargetType());
        String uri = SSTUtils.getNormalizedUri(oDataContext);
        ODataResultList<?> entityList = gttCoreServiceClient.readEntitySet(uri, clazz);
        return convertResults(entityList);
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        Class<?> clazz = getClazz(uriInfo.getTargetType());
        String uri = SSTUtils.getNormalizedUri(oDataContext);
        Object entity = gttCoreServiceClient.readEntity(uri, clazz);
        return ODataUtils.toMap(entity);
    }

    @Override
    public Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SSTUtils.getNormalizedUri(oDataContext);
        return gttCoreServiceClient.countEntitySet(uri);
    }

    protected ODataResultList<Map<String, Object>> convertResults(ODataResultList<?> entityList) {
        ODataResultList<Map<String, Object>> res = new ODataResultList<>();
        res.setCount(entityList.getCount());
        for (Object entity : entityList.getResults()) {
            res.getResults().add(ODataUtils.toMap(entity));
        }
        return res;
    }

    protected Class<?> getClass(EdmType targetType) {
        try {
            return Class.forName(getDefaultPackageName() + "." + targetType.getName());
        } catch (ClassNotFoundException | EdmException e) {
            logger.error("get class type of {} failed.", targetType, e);
            throw new SSTServiceException("get class type failed.");
        }
    }

    private String getDefaultPackageName() {
        return ProcessStatus.class.getPackage().getName();
    }

    private Class<?> getClazz(EdmType targetType) {
        String targetTypeName = getTargetName(targetType);
        return map.containsKey(targetTypeName)
                ? map.get(targetTypeName)
                : getClass(targetType);
    }
}
