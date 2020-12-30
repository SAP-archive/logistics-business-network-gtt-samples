package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.odata.constant.ODataConstants.EXPAND;
import static java.util.Collections.emptyList;

import com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument;
import com.sap.gtt.v2.sample.sst.odata.model.Shipment;
import com.sap.gtt.v2.sample.sst.odata.service.ShipmentService;
import com.sap.gtt.v2.sample.sst.rest.model.converter.CarrierReferenceDocumentConverter;
import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
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
public class CarrierReferenceDocumentServiceImpl implements CarrierReferenceDocumentService {

    private static final String CARRIER_REF_DOCUMENTS_PARAM = "carrierRefDocuments";

    @Autowired
    private ShipmentService shipmentService;

    @Autowired
    private CarrierReferenceDocumentConverter carrierReferenceDocumentConverter;

    @Override
    public List<CarrierReferenceDocumentDto> getByShipmentId(@NotNull final String shipmentId) {
        final MultiValueMap<String, String> params = buildParams();
        final Optional<Shipment> shipmentOpt = shipmentService.getById(shipmentId, params);
        return shipmentOpt.map(this::mapToCarrierReferenceDocumentDtos).orElse(emptyList());
    }

    private List<CarrierReferenceDocumentDto> mapToCarrierReferenceDocumentDtos(final Shipment shipment) {
        final List<CarrierReferenceDocument> carrierRefDocuments = shipment.getCarrierRefDocuments();
        final String shipmentNo = shipment.getShipmentNo();
        final List<CarrierReferenceDocumentDto> dtos = carrierReferenceDocumentConverter.toDtoList(carrierRefDocuments);
        dtos.forEach(it -> it.setShipmentNo(shipmentNo));
        return dtos;
    }

    private MultiValueMap<String, String> buildParams() {
        final MultiValueMap<String, String> params = new LinkedMultiValueMap<>();
        params.add(EXPAND, CARRIER_REF_DOCUMENTS_PARAM);
        return params;
    }
}
