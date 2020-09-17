package com.sap.gtt.v2.sample.sof.odata.handler;


import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.StringUtils;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;

@Component
public class ArrivalTimeOdataHandler extends SOFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(ArrivalTimeOdataHandler.class);
    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }


    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }

    public List<ArrivalTime> getArrivalTimes4DeliveryItem(UUID processId,DeliveryItem deliveryItem) {
        String locationAltKey = SOFUtils.generateLocationAltKey(deliveryItem.getPartyId(),deliveryItem.getLogicalSystem(),deliveryItem.getDestinationLocationTypeCode(),deliveryItem.getDestination());
        String url = getPlannedEvent4DeliveryItemUrl(processId,locationAltKey);
        ODataResultList<PlannedEvent> oDataResultList = gttCoreServiceClient.readEntitySet(url, PlannedEvent.class);

        List<ArrivalTime> arrivalTimes = new LinkedList<>();
        oDataResultList.getResults().stream().filter(plannedEvent-> plannedEvent.getLastProcessEventDirectory()!=null&&plannedEvent.getLastProcessEventDirectory().getEvent()!=null&& plannedEvent.getLastProcessEventDirectory().getEvent().getActualBusinessTimestamp()!=null).forEach(plannedEvent -> {
            ArrivalTime arrivalTime = new ArrivalTime();
            arrivalTime.setId(UUID.randomUUID());
            arrivalTime.setProcessId(processId);
            arrivalTime.setActualBizTs(plannedEvent.getLastProcessEventDirectory().getEvent().getActualBusinessTimestamp());
            arrivalTime.setPlannedBizTs(plannedEvent.getPlannedBusinessTimestamp());
            arrivalTime.setPlannedBizTsEarliest(plannedEvent.getPlannedBizTsEarliest());
            arrivalTimes.add(arrivalTime);
        });
        deliveryItem.setArrivalTimes(arrivalTimes);
        return arrivalTimes;
    }

    public String getPlannedEvent4DeliveryItemUrl(UUID processId,String locationAltKey) {
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, processId.toString(), BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        propertyCondition = new FilterCondition("locationAltKey", FilterCondition.EDM_TYPE_STRING, locationAltKey, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        String targetEntityName = Constants.PLANNED_EVENT_ENTITY_NAME;
        String filter = "and (substringof('"+Constants.SHIPMENT_ARRIVAL+"',eventType)) ";
        List<String> expand = new ArrayList<>();
        expand.add("lastProcessEventDirectory/event");
        return SOFUtils.generateUrl(targetEntityName, filter,filterConditions,BinaryOperator.AND,false,false, expand, null);
    }

    public void getArrivalTimes4DeliveryItems(List<DeliveryItem> deliveryItems) {
        if(deliveryItems==null) {
            return;
        }

        for(DeliveryItem deliveryItem : deliveryItems) {
            getArrivalTimes4DeliveryItem(deliveryItem.getId(),deliveryItem);
        }
    }
}
