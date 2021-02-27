package com.sap.gtt.v2.sample.sst.odata.helper;

import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.Delivery;
import com.sap.gtt.v2.sample.sst.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sst.odata.model.DeliveryTp;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link DeliveryItemHelper} is a helper class for {@link DeliveryItem} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class DeliveryItemHelper {

    /**
     * Retrieves {@link DeliveryItem} entities from provided {@link Shipment}.
     *
     * @param shipment - {@link Shipment} entity
     * @return list of {@link DeliveryItem}
     */
    public List<DeliveryItem> retrieveFromShipment(@NotNull final Shipment shipment) {
        final List<DeliveryTp> deliveryTps = shipment.getDeliveryTps();
        return mapToDeliveryItems(deliveryTps);
    }

    private List<DeliveryItem> mapToDeliveryItems(final List<DeliveryTp> deliveryTps) {
        return deliveryTps.stream()
                .map(DeliveryTp::getDelivery)
                .map(Delivery::getDeliveryItems)
                .flatMap(List::stream)
                .collect(toList());
    }
}
