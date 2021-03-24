package com.sap.gtt.v2.sample.pof.rest.service.forward;

import com.google.gson.JsonObject;
import com.sap.gtt.v2.sample.pof.constant.Constants;
import com.sap.gtt.v2.sample.pof.odata.model.InboundDeliveryItem;
import com.sap.gtt.v2.sample.pof.odata.model.PurchaseOrder;
import com.sap.gtt.v2.sample.pof.utils.ForwardUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static com.sap.gtt.v2.sample.pof.constant.Constants.DELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.constant.Constants.UNDELETION_EVENT_ENTITY_NAME;
import static com.sap.gtt.v2.sample.pof.utils.POFUtils.isEventTypesEqual;
import static org.apache.commons.lang3.StringUtils.isBlank;

@Service
public class ForwardFacadeService {

    @Autowired
    private InternalCompleteValueService internalCompleteValueService;
    @Autowired
    private InternalExecutionStatusService internalExecutionStatusService;
    @Autowired
    private InternalLastActivityService internalLastActivityService;
    @Autowired
    private InternalDelayedService internalDelayedService;
    @Autowired
    private InternalDelayImpactService internalDelayImpactService;
    @Autowired
    private InternalCompletedAndLateService internalCompletedAndLateService;

    public void updateProperties(PurchaseOrder purchaseOrder, Optional<InboundDeliveryItem> deliveryItemOptional, String eventType) {
        if (deliveryItemOptional.isPresent()) {
            internalDelayedService.processIsDelayed(purchaseOrder);
            internalDelayImpactService.updateDelayImpact(purchaseOrder);
            internalCompletedAndLateService.updateCompletedAndLate(purchaseOrder);

            InboundDeliveryItem inboundDeliveryItem = deliveryItemOptional.get();
            if (isEventTypesEqual(Constants.GOODS_RECEIPT_EVENT, eventType)) {
                internalExecutionStatusService.updateForNotPODInboundDelivery(inboundDeliveryItem);
            }
        }
        boolean isEventForFlow = ForwardUtils.isActualEventForFlow(eventType);
        if (isEventForFlow || isDeletionUndeletionEvent(eventType)) {
            internalCompleteValueService.recalculateCompletionValue(purchaseOrder);
        }
    }

    public void updateLastActivity(JsonObject trackedProcess, InboundDeliveryItem inboundDeliveryItem) {
        internalLastActivityService.updateLastActivityOfDeliveryItem(trackedProcess, inboundDeliveryItem);
    }

    private boolean isDeletionUndeletionEvent(String eventType) {
        return !isBlank(eventType) && (eventType.endsWith(DELETION_EVENT_ENTITY_NAME) || eventType.endsWith(UNDELETION_EVENT_ENTITY_NAME));
    }
}
