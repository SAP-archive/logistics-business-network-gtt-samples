package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;

import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.converter.ShipmentStopsForVpConverter;
import java.util.List;
import java.util.Optional;
import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.validation.annotation.Validated;

/**
 * @author Aliaksandr Miron
 */
@Validated
@Service
public class ShipmentStopsForVpService extends StopsForVpAbstractService<Shipment> {

    @Autowired
    private ShipmentService shipmentService;

    @Autowired
    private ShipmentStopsForVpConverter shipmentStopsForVpConverter;

    @Override
    protected Optional<Shipment> getTrackedProcess(@NotNull final String shipmentId) {
        final MultiValueMap<String, String> params = buildShipmentParams();
        return shipmentService.getById(shipmentId, params);
    }

    @Override
    protected List<StopsForVp> convertFromTrackedProcess(@NotNull final Shipment shipment) {
        return shipmentStopsForVpConverter.fromShipment(shipment);
    }

    private MultiValueMap<String, String> buildShipmentParams() {
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add(EXPAND, STOPS_FOR_VP_FIELD);
        return params;
    }
}
