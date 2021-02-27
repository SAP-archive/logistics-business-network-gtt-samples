package com.sap.gtt.v2.sample.sst.odata.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;

import com.sap.gtt.v2.sample.sst.odata.model.FreightUnit;
import com.sap.gtt.v2.sample.sst.odata.model.StopsForVp;
import com.sap.gtt.v2.sample.sst.rest.model.converter.FreightUnitStopsForVpConverter;
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
public class FreightUnitStopsForVpService extends StopsForVpAbstractService<FreightUnit> {

    @Autowired
    private FreightUnitService freightUnitService;

    @Autowired
    private FreightUnitStopsForVpConverter freightUnitStopsForVpConverter;

    @Override
    protected Optional<FreightUnit> getTrackedProcess(@NotNull final String freightUnitId) {
        final MultiValueMap<String, String> params = buildFreightUnitParams();
        return freightUnitService.getById(freightUnitId, params);
    }

    @Override
    protected List<StopsForVp> convertFromTrackedProcess(@NotNull final FreightUnit freightUnit) {
        return freightUnitStopsForVpConverter.fromFreightUnit(freightUnit);
    }

    private MultiValueMap<String, String> buildFreightUnitParams() {
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add(EXPAND, STOPS_FOR_VP_FIELD);
        return params;
    }
}
