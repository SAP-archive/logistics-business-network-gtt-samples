package com.sap.gtt.v2.sample.sst.odata.helper;

import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnitTp;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.ShipmentTp;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link FreightUnitHelper} is a helper class for {@link FreightUnit} entities.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class FreightUnitHelper {

    /**
     * Retrieves {@link FreightUnit} entities from provided {@link Shipment}.
     *
     * @param shipment - {@link Shipment} entity
     * @return list of {@link FreightUnit}
     */
    public List<FreightUnit> retrieveFromShipment(@NotNull final Shipment shipment) {
        final List<FreightUnitTp> freightUnitTps = shipment.getFreightUnitTps();
        return mapToFreightUnits(freightUnitTps);
    }

    /**
     * Retrieves {@link Shipment} entities from provided {@link FreightUnit}.
     *
     * @param freightUnit - {@link FreightUnit} entity
     * @return list of {@link Shipment}
     */
    public List<Shipment> retrieveShipments(@NotNull final FreightUnit freightUnit) {
        final List<ShipmentTp> shipmentTps = freightUnit.getShipmentTps();
        return mapToShipments(shipmentTps);
    }

    private List<Shipment> mapToShipments(final List<ShipmentTp> shipmentTps) {
        return shipmentTps.stream()
                .map(ShipmentTp::getShipment)
                .collect(toList());
    }

    private List<FreightUnit> mapToFreightUnits(final List<FreightUnitTp> freightUnitTps) {
        return freightUnitTps.stream()
                .map(FreightUnitTp::getFreightUnit)
                .collect(toList());
    }
}
