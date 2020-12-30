package com.sap.gtt.v2.sample.pof.rest.service.trackingTimeline;

import com.sap.gtt.v2.sample.pof.domain.ProcessEventDirectory;

import javax.validation.constraints.NotNull;
import java.util.List;

public interface ProcessEventDirectoryService {
    List<ProcessEventDirectory> getAllByDeliveryItemId(@NotNull String deliveryItemId);

    List<ProcessEventDirectory> getWithoutPlannedEvent(
            @NotNull List<ProcessEventDirectory> processEventDirectories);

}
