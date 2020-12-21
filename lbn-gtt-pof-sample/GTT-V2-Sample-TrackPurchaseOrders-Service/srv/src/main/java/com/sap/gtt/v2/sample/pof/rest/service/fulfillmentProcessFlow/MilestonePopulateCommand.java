package com.sap.gtt.v2.sample.pof.rest.service.fulfillmentProcessFlow;

import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrderItem;
import com.sap.gtt.v2.sample.pof.rest.domain.fulfillmentprocessflow.Lane;

public interface MilestonePopulateCommand {
    void populateMilestone(Lane lane, PurchaseOrderItem item);
}
