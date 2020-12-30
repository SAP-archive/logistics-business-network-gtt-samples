package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.SalesOrder;
import com.sap.gtt.v2.sample.sof.service.client.GTTCoreServiceClient;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
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

import static com.sap.gtt.v2.sample.sof.utils.SOFUtils.getTargetName;

@Component
public class SOFDefaultODataHandler implements SOFODataHandler {
    private static Logger logger = LoggerFactory.getLogger(SOFDefaultODataHandler.class);
    @Autowired
    protected GTTCoreServiceClient gttCoreServiceClient;

    private static Map<String, Class<?>> map = new ConcurrentHashMap<>();
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

        String uri = SOFUtils.getNormalizedUri(oDataContext);

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

        String uri = SOFUtils.getNormalizedUri(oDataContext);
        Object obj = gttCoreServiceClient.readEntity(uri, clazz);

        return ODataUtils.toMap(obj);
    }

    @Override
    public Integer handleCountEntitySet(GetEntitySetCountUriInfo uriInfo, ODataContext oDataContext) {
        String uri = SOFUtils.getNormalizedUri(oDataContext);
        Integer count= gttCoreServiceClient.countEntitySet(uri);
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
            throw new SOFServiceException("get class type failed.");
        }
    }

    private String getDefaultPackageName() {
        return SalesOrder.class.getPackage().getName();
    }

}
