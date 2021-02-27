package com.sap.gtt.v2.sample.sst.odata.helper;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.List;
import org.junit.jupiter.api.Test;

class FreightUnitHelperTest {

    private final FreightUnitHelper freightUnitHelper = new FreightUnitHelper();

    @Test
    void retrieveFromShipment_givenShipment_shouldReturnFreightUnits() {
        // given
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        // when
        final List<FreightUnit> freightUnits = freightUnitHelper.retrieveFromShipment(shipment);

        // then
        assertThat(freightUnits).isNotEmpty();
    }

    @Test
    void retrieveShipments_givenFreightUnit_shouldReturnShipments() {
        // given
        final String freightUnitJson = getStringFromResource("/odata/freight-unit.json");
        final FreightUnit freightUnit = ODataUtils.readEntity(freightUnitJson, FreightUnit.class);

        // when
        final List<Shipment> shipments = freightUnitHelper.retrieveShipments(freightUnit);

        // then
        assertThat(shipments).isNotEmpty();
    }
}
