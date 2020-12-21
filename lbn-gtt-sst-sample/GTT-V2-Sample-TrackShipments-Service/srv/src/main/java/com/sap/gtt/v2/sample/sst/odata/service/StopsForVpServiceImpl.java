package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static java.util.Collections.emptyList;

import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.converter.StopsForVpConverter;
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
public class StopsForVpServiceImpl implements StopsForVpService {

    private static final String STOPS_FOR_VP_FIELD = "stopsForVP";

    @Autowired
    private ShipmentService shipmentService;

    @Autowired
    private StopsForVpConverter stopsForVpConverter;

    @Override
    public List<StopsForVp> getAll(@NotNull final String shipmentId) {
        final Optional<Shipment> shipmentOpt = getShipment(shipmentId);
        return shipmentOpt.isPresent()
                ? stopsForVpConverter.fromShipment(shipmentOpt.get())
                : emptyList();
    }

    private Optional<Shipment> getShipment(final String shipmentId) {
        final MultiValueMap<String, String> params = buildShipmentParams();
        return shipmentService.getById(shipmentId, params);
    }

    private MultiValueMap<String, String> buildShipmentParams() {
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add(EXPAND, STOPS_FOR_VP_FIELD);
        return params;
    }
}
