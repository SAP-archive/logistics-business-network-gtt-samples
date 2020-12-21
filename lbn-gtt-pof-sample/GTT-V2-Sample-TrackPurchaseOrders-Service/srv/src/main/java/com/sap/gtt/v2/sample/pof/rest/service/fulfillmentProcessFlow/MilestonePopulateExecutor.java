package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.constant.FulfillmentProcessMilestoneEnum;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;

@Component
public class MilestonePopulateExecutor {

    private final Map<FulfillmentProcessMilestoneEnum, MilestonePopulateCommand> commandByMilestoneEnum;

    public MilestonePopulateExecutor(Map<FulfillmentProcessMilestoneEnum, MilestonePopulateCommand> commandByMilestoneEnum) {
        this.commandByMilestoneEnum = commandByMilestoneEnum;
    }

    public void execute(Lane lane, PurchaseOrderItem item) {
        FulfillmentProcessMilestoneEnum milestone = FulfillmentProcessMilestoneEnum.getMilestoneByName(lane.getName());
        MilestonePopulateCommand command = commandByMilestoneEnum.get(milestone);
        Optional.ofNullable(command).ifPresent(it -> it.populateMilestone(lane, item));
    }
}
