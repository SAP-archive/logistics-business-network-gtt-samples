package com.sap.gtt.v2.sample.sof.odata.handler;

import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.edm.EdmType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static com.sap.gtt.v2.sample.sof.odata.handler.SalesOrderODataHandler.SOF_DEFAULT_O_DATA_HANDLER;
import static com.sap.gtt.v2.sample.sof.utils.SOFUtils.getTargetName;

@Component
public class ODataHandlerFactory {
    private static Logger logger = LoggerFactory.getLogger(ODataHandlerFactory.class);

    @Autowired
    private ApplicationContext applicationContext;


    private static Map<String, Class<?>> map = new ConcurrentHashMap<>();

    static {
        map.put(Constants.MODEL_NAMESPACE + ".SalesOrder", SalesOrderODataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".SalesOrderItem", SOIOdataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".DeliveryItem", DeliveryItemOdataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".ArrivalTime", ArrivalTimeOdataHandler.class);
        map.put(Constants.MODEL_NAMESPACE + ".LocationDTO",LocationDTOOdataHandler.class);

    }

    public void register(String targetType, Class<?> handlerType) {
        map.put(targetType, handlerType);
    }

    public SOFODataHandler getHandler(EdmType edmType) {
        String targetType = getTargetName(edmType);
        if (!map.containsKey(targetType)) {
            logger.debug("OData handler for {} not defined, will fallback on default handler", edmType);
            return (SOFODataHandler)applicationContext.getBean(SOF_DEFAULT_O_DATA_HANDLER);
        }
        return (SOFODataHandler)applicationContext.getBean(map.get(targetType));
    }

}
