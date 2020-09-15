package com.sap.gtt.v2.sample.sof.odata.model;

import com.google.gson.annotations.SerializedName;
import com.sap.gtt.v2.sample.sof.constant.Constants;
import org.apache.olingo.odata2.api.annotation.edm.*;

import java.util.UUID;

@EdmEntitySet(name = ArrivalTime.ENTITY_SET_NAME , container = Constants.ENTITY_CONTAINER_NAME)
@EdmEntityType(namespace = Constants.MODEL_NAMESPACE)
public class ArrivalTime {
    public static final String ENTITY_SET_NAME = "ArrivalTime";
    @EdmKey
    @EdmProperty(name = "id")
    private UUID id;

    @EdmProperty(name = "processId")
    private UUID processId;

    @EdmProperty(name = "actualBizTs",type = EdmType.DATE_TIME_OFFSET)
    private Long actualBizTs;

    @EdmProperty(name = "plannedBizTs",type = EdmType.DATE_TIME_OFFSET)
    private Long plannedBizTs;

    @EdmProperty(name = "plannedBizTsEarliest",type = EdmType.DATE_TIME_OFFSET)
    private Long plannedBizTsEarliest;


    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public UUID getProcessId() {
        return processId;
    }

    public void setProcessId(UUID processId) {
        this.processId = processId;
    }

    public Long getActualBizTs() {
        return actualBizTs;
    }

    public void setActualBizTs(Long actualBizTs) {
        this.actualBizTs = actualBizTs;
    }

    public Long getPlannedBizTs() {
        return plannedBizTs;
    }

    public void setPlannedBizTs(Long plannedBizTs) {
        this.plannedBizTs = plannedBizTs;
    }

    public Long getPlannedBizTsEarliest() {
        return plannedBizTsEarliest;
    }

    public void setPlannedBizTsEarliest(Long plannedBizTsEarliest) {
        this.plannedBizTsEarliest = plannedBizTsEarliest;
    }
}
