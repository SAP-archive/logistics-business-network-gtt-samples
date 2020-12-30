package com.sap.gtt.v2.sample.sof.odata.handler;


import com.sap.gtt.v2.sample.sof.constant.Constants;
import com.sap.gtt.v2.sample.sof.domain.OrderBy;
import com.sap.gtt.v2.sample.sof.domain.PlannedEvent;
import com.sap.gtt.v2.sample.sof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.sof.exception.SOFServiceException;
import com.sap.gtt.v2.sample.sof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.sof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.sof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.sof.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sof.service.SOFService;
import com.sap.gtt.v2.sample.sof.utils.ODataUtils;
import com.sap.gtt.v2.sample.sof.utils.SOFUtils;
import org.apache.commons.collections.CollectionUtils;
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

import java.util.*;
import java.util.stream.Collectors;

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
        ODataResultList<ProcessEventDirectory> oDataResultList = gttCoreServiceClient.readEntitySet(url, ProcessEventDirectory.class);
        List<ArrivalTime> arrivalTimes = getArrivalTimes(oDataResultList.getResults());
        deliveryItem.setArrivalTimes(arrivalTimes);
        return arrivalTimes;
    }

    public List<ArrivalTime> getArrivalTimes(List<ProcessEventDirectory> oDataResultList) {
        List<ArrivalTime> arrivalTimes = new LinkedList<>();
        if(CollectionUtils.isEmpty(oDataResultList)) {
            return arrivalTimes;
        }
        oDataResultList.stream().filter(processEventDirectory -> processEventDirectory.getPlannedEventId()!=null).collect(Collectors.groupingBy(ProcessEventDirectory::getPlannedEventId)).forEach((plannedEventId, peds)->{
            if(CollectionUtils.isNotEmpty(peds)) {
                ArrivalTime arrivalTime = new ArrivalTime();
                ProcessEventDirectory ped = peds.get(0);
                arrivalTime.setId(UUID.randomUUID());
                arrivalTime.setActualBizTs(ped.getEvent().getActualBusinessTimestamp());
                arrivalTime.setPlannedBizTs(ped.getPlannedEvent().getPlannedBusinessTimestamp());
                arrivalTime.setPlannedBizTsEarliest(ped.getPlannedEvent().getPlannedBizTsEarliest());
                arrivalTime.setProcessId(ped.getProcessId());
                arrivalTimes.add(arrivalTime);
            }
        });
        return arrivalTimes;
    }

    public String getPlannedEvent4DeliveryItemUrl(UUID processId,String locationAltKey) {

        //gtt-core-outbound-readservice-acc.cfapps.sap.hana.ondemand.com/sap/logistics/gtt/outbound/odata/v1/com.lbngttsamples.gtt.app.sof.sofService/ProcessEventDirectory?$expand=plannedEvent,event&$filter=process_id eq guid'506a418d-958d-5eb0-844a-e4eb3e22c001' and plannedEvent/locationAltKey eq 'xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:Customer:LBN_CUS_CL' and substringof('Shipment.Arrival',plannedEvent/eventType) and (correlationType_code eq 'EARLY_REPORTED' or correlationType_code eq 'REPORTED' or correlationType_code eq 'LATE_REPORTED')&$orderby=event/actualBusinessTimestamp desc&$top=1&$format=json
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition = null;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, processId.toString(), BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        propertyCondition = new FilterCondition("plannedEvent/locationAltKey", FilterCondition.EDM_TYPE_STRING, locationAltKey, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        String targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        String filter = "and (substringof('"+Constants.SHIPMENT_ARRIVAL+"',plannedEvent/eventType)) ";
        List<String> expand = new ArrayList<>();
        expand.add("event");
        expand.add("plannedEvent");
        List<OrderBy> orders = new LinkedList<>();
        orders.add(new OrderBy("event/actualBusinessTimestamp","desc"));
        filter = filter + " and " +SOFUtils.getReportedCorrelationTypeCode();
        return SOFUtils.generateUrl(targetEntityName, filter,filterConditions,BinaryOperator.AND,false,false, expand, orders);
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
