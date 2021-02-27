package com.sap.gtt.v2.sample.sst.odata.helper;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.odata.model.DeliveryItem;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import java.util.List;
import org.junit.jupiter.api.Test;

class DeliveryItemHelperTest {

    private final DeliveryItemHelper deliveryItemHelper = new DeliveryItemHelper();

    @Test
    void retrieveFromShipment_givenShipment_shouldRetrieveDeliveryItems() {
        // given
        final String shipmentJson = getStringFromResource("/odata/shipment.json");
        final Shipment shipment = ODataUtils.readEntity(shipmentJson, Shipment.class);

        // when
        final List<DeliveryItem> deliveryItems = deliveryItemHelper.retrieveFromShipment(shipment);

        // then
        assertThat(deliveryItems).isNotEmpty();
    }
}