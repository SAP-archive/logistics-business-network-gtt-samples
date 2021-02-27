package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;

/***
 * MilestonePopulateExecutor.class is an entry point for Command pattern base lane processing logic.
 * All commands configure by their associated Milestone name in MilestonePopulateExecutorConfiguration class
 */
@Component
public class MilestonePopulateExecutor {

    @Autowired
    private Map<FulfillmentProcessMilestoneEnum, MilestonePopulateCommand> commandByMilestoneEnum;

    public void execute(Lane lane, PurchaseOrderItem item) {
        FulfillmentProcessMilestoneEnum milestone = FulfillmentProcessMilestoneEnum.getMilestoneByName(lane.getName());
        MilestonePopulateCommand command = commandByMilestoneEnum.get(milestone);
        Optional.ofNullable(command).ifPresent(it -> it.populateMilestone(lane, item));
    }
}
