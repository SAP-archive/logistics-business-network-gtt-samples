package com.sap.gtt.v2.sample.sst.odata.handler;

import static com.sap.gtt.v2.sample.sst.common.constant.Constants.MODEL_NAMESPACE;

import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.Location;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * {@link ODataHandlerFactory} is a factory which registers required class as OData handler.
 *
 * @author Min Li
 */
@Component
public class ODataHandlerFactory {

    private static final Logger logger = LoggerFactory.getLogger(ODataHandlerFactory.class);
    private static final Map<String, Class<?>> map = new ConcurrentHashMap<>();

    @Autowired
    private ApplicationContext applicationContext;

    static {
        map.put(MODEL_NAMESPACE + "." + Shipment.ENTITY_SET_NAME, ShipmentODataHandler.class);
        map.put(MODEL_NAMESPACE + "." + Location.ENTITY_SET_NAME, LocationODataHandler.class);
    }

    /**
     * Registers provided class as OData handler.
     *
     * @param targetType  - name of target type
     * @param handlerType - class of entity
     */
    public void register(String targetType, Class<?> handlerType) {
        map.put(targetType, handlerType);
    }

    /**
     * Retrieves OData handler by provided EDM type.
     *
     * @param edmType - EDM type of entity
     * @return implementation of {@link SSTODataHandler} if exists, otherwise - {@link SSTDefaultODataHandler}
     */
    public SSTODataHandler getHandler(EdmType edmType) {
        String targetType = SSTUtils.getTargetName(edmType);
        if (!map.containsKey(targetType)) {
            logger.debug("OData handler for {} not defined, will fallback on default handler", edmType);
            return applicationContext.getBean(SSTDefaultODataHandler.class);
        }
        return (SSTODataHandler) applicationContext.getBean(map.get(targetType));
    }
}
