package com.sap.gtt.v2.sample.pof.odata.handler;

import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.domain.OrderBy;
import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;
import com.sap.gtt.v2.sample.pof.odata.filter.FilterCondition;
import com.sap.gtt.v2.sample.pof.odata.helper.ODataResultList;
import com.sap.gtt.v2.sample.pof.odata.model.ArrivalTime;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.utils.POFUtils;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.olingo.odata2.api.processor.ODataContext;
import org.apache.olingo.odata2.api.uri.expression.BinaryOperator;
import org.apache.olingo.odata2.api.uri.info.GetEntitySetUriInfo;
import org.apache.olingo.odata2.api.uri.info.GetEntityUriInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@Component
public class ArrivalTimeODataHandler extends POFDefaultODataHandler {
    private static final Logger logger = LoggerFactory.getLogger(ArrivalTimeODataHandler.class);

    @Override
    public ODataResultList<Map<String, Object>> handleReadEntitySet(GetEntitySetUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }

    @Override
    public Map<String, Object> handleReadEntity(GetEntityUriInfo uriInfo, ODataContext oDataContext) {
        throw new NotImplementedException();
    }

    public List<ArrivalTime> getArrivalTimes4DeliveryItem(UUID deliveryItemId, String locationAltKey) {
        logger.info("get all arrival items by {}", deliveryItemId);
        String url = getPlannedEvent4DeliveryItemUrl(deliveryItemId, locationAltKey);
        ODataResultList<ProcessEventDirectory> oDataResultList = gttCoreServiceClient.readEntitySetAll(url, ProcessEventDirectory.class);
        return getArrivalTimes(oDataResultList.getResults());
    }

    private List<ArrivalTime> getArrivalTimes(List<ProcessEventDirectory> oDataResultList) {
        return oDataResultList.stream()
                .filter(processEventDirectory -> processEventDirectory.getPlannedEventId() != null)
                .filter(processEventDirectory -> processEventDirectory.getEvent() != null)
                .collect(Collectors.groupingBy(ProcessEventDirectory::getPlannedEventId))
                .values().stream()
                .map(processEventDirectories -> processEventDirectories.stream()
                        .findAny()
                        .flatMap(planned -> processEventDirectories.stream()
                                .filter(it -> it.getId().equals(planned.getPlannedEvent().getLastProcessEventDirectoryId()))
                                .findFirst()
                                .map(ped -> {
                                    ArrivalTime arrivalTime = new ArrivalTime();
                                    arrivalTime.setId(UUID.randomUUID());
                                    arrivalTime.setActualBizTs(ped.getEvent().getActualBusinessTimestamp());
                                    arrivalTime.setPlannedBizTs(ped.getPlannedEvent().getPlannedBusinessTimestamp());
                                    arrivalTime.setPlannedBizTsEarliest(ped.getPlannedEvent().getPlannedBizTsEarliest());
                                    arrivalTime.setPlannedBizTsLatest(ped.getPlannedEvent().getPlannedBizTsLatest());
                                    arrivalTime.setProcessId(ped.getProcessId());
                                    return arrivalTime;
                                })
                        ).orElse(null)
                ).filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private String getPlannedEvent4DeliveryItemUrl(UUID processId, String locationAltKey) {

        // {base_url}/ProcessEventDirectory?$expand=plannedEvent,event&$filter=process_id eq guid'<id>' and plannedEvent/locationAltKey eq 'xri://sap.com/id:LBN%2310010001016:QM7CLNT910:Location:Customer:LBN_CUS_CL' and substringof('Shipment.Arrival',plannedEvent/eventType) and (correlationType_code eq 'EARLY_REPORTED' or correlationType_code eq 'REPORTED' or correlationType_code eq 'LATE_REPORTED')&$orderby=event/actualBusinessTimestamp desc&$top=1&$format=json
        List<FilterCondition> filterConditions = new ArrayList<>();
        FilterCondition propertyCondition;
        propertyCondition = new FilterCondition("process_id", FilterCondition.EDM_TYPE_GUID, processId.toString(), BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        propertyCondition = new FilterCondition("plannedEvent/locationAltKey", FilterCondition.EDM_TYPE_STRING, locationAltKey, BinaryOperator.EQ);
        filterConditions.add(propertyCondition);
        String targetEntityName = Constants.PROCESS_EVENT_DIRECTORY_ENTITY_NAME;
        String filter = "and (substringof('" + Constants.SHIPMENT_ARRIVAL + "',plannedEvent/eventType)) ";
        List<String> expand = new ArrayList<>();
        expand.add("event");
        expand.add("plannedEvent");
        List<OrderBy> orders = new LinkedList<>();
        orders.add(new OrderBy("event/actualBusinessTimestamp", "desc"));
        filter = filter + " and " + POFUtils.getReportedCorrelationTypeCode();
        return POFUtils.generateUrl(targetEntityName, filter, filterConditions, BinaryOperator.AND, false, false, expand, orders);
    }

    public Map<UUID, List<ArrivalTime>> getArrivalTimes4DeliveryItems(List<InboundDeliveryItem> deliveryItems) {
        Map<UUID, List<ArrivalTime>> result = new HashMap<>();
        for (InboundDeliveryItem deliveryItem : deliveryItems) {
            String locationAltKey = POFUtils.generateLocationAltKey(deliveryItem.getPartyId(), deliveryItem.getLogicalSystem(),
                    deliveryItem.getPlantLocationTypeCode(), deliveryItem.getPlant());
            List<ArrivalTime> arrivalTimes4DeliveryItem = getArrivalTimes4DeliveryItem(deliveryItem.getId(), locationAltKey);
            result.put(deliveryItem.getId(), arrivalTimes4DeliveryItem);
        }
        return result;
    }
}