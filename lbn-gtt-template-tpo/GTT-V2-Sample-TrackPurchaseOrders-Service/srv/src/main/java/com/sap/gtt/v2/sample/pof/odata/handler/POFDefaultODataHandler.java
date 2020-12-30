package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.exception.POFServiceException;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.ProcessStatus;
import com.sap.gtt.v2.sample.pof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.pof.utils.ODataUtils;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.olingo.odata2.api.edm.EdmException;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetCountUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static com.sap.gtt.v2.sample.pof.utils.POFUtils.getTargetName;

@Component
public class POFDefaultODataHandler implements POFODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(POFDefaultODataHandler.class);

    public static final String POF_DEFAULT_O_DATA_HANDLER = "POFDefaultODataHandler";

    @Autowired
    protected GTTCoreServiceClient gttCoreServiceClient;

    private static final Map<String, Class<?>> map = new ConcurrentHashMap<>();
    static {
        /* if new Edm entity is in default package, no need to add explicit mapping
           enable following code only when Edm entity is not in default package
            // map.put(Constants.MODEL_NAMESPACE + ".Dummy", Dummy.class);
        */
    }

    @Override
    public void register(String targetType, Class<?> clazz) {
        map.put(targetType, clazz);
    }

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext){
        Class<?> clazz = getClazz(uriInfo.getTargetType());

        String uri = POFUtils.getNormalizedUri(oDataContext);

        ODataResultList entityList = gttCoreServiceClient.readEntitySet(uri, clazz);
        return convertResults(entityList);
    }

    private Class<?> getClazz(EdmType targetType) {
        String targetTypeName = getTargetName(targetType);
        Class<?> clazz;
        if (map.containsKey(targetTypeName)) {
            clazz = map.get(targetTypeName);
        } else {
            clazz = getClass(targetType);
        }
        return clazz;
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext){
        Class<?> clazz = getClazz(uriInfo.getTargetType());

        String uri = POFUtils.getNormalizedUri(oDataContext);
        Object obj = gttCoreServiceClient.readEntity(uri, clazz);

        return ODataUtils.toMap(obj);
    }

    @Override
    public Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext) {
        String uri = POFUtils.getNormalizedUri(oDataContext);
        Integer count = gttCoreServiceClient.countEntitySet(uri);
        return count;
    }

    protected ODataResultList<Map<String, Object>> convertResults(ODataResultList entityList) {
        ODataResultList<Map<String, Object>> res = new ODataResultList<>();
        res.setCount(entityList.getCount());
        for(Object entity : entityList.getResults()) {
            res.getResults().add(ODataUtils.toMap(entity));
        }
        return res;
    }

    protected Class<?> getClass(EdmType targetType) {
        try {
            return Class.forName(getDefaultPackageName() + "." + targetType.getName());
        } catch (ClassNotFoundException | EdmException e) {
            logger.error("get class type of {} failed.", targetType, e);
            throw new POFServiceException("get class type failed.");
        }
    }

    private String getDefaultPackageName() {
        return ProcessStatus.class.getPackage().getName();
    }

}
