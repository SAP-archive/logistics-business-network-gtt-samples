package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static com.sap.gtt.v2.sample.pof.odata.handler.POFDefaultODataHandler.POF_DEFAULT_O_DATA_HANDLER;
import static com.sap.gtt.v2.sample.pof.utils.POFUtils.getTargetName;

@Component
public class ODataHandlerFactory {
    private static final Logger logger = LoggerFactory.getLogger(ODataHandlerFactory.class);
    private static final Map<String, Class<?>> map = new ConcurrentHashMap<>();

    private final ApplicationContext applicationContext;

    static {
        map.put(Constants.MODEL_NAMESPACE + ".PurchaseOrder", POFPurchaseOrderODataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".PurchaseOrderItem", POFPurchaseOrderItemODataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".InboundDeliveryItem", POFInboundDeliveryItemODataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".ArrivalTime", ArrivalTimeODataHandler.class);
    }

    public ODataHandlerFactory(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    public void register(String targetType, Class<?> handlerType) {
        map.put(targetType, handlerType);
    }

    public POFODataHandler getHandler(EdmType edmType) {
        String targetType = getTargetName(edmType);
        if (!map.containsKey(targetType)) {
            logger.debug("OData handler for {} not defined, will fallback on default handler", edmType);
            return (POFODataHandler) applicationContext.getBean(POF_DEFAULT_O_DATA_HANDLER);
        }
        return (POFODataHandler) applicationContext.getBean(map.get(targetType));
    }

}
