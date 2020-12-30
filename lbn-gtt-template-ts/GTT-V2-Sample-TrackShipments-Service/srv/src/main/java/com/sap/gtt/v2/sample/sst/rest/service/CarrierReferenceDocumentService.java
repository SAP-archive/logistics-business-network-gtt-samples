package com.sap.gtt.v2.sample.sst.rest.service;

import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import java.util.List;
import javax.validation.constraints.NotNull;

/**
 * {@link CarrierReferenceDocumentService} is a service which operates on
 * {@link com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument} entities.
 *
 * @author Aliaksandr Miron
 */
public interface CarrierReferenceDocumentService {

    /**
     * Retrieves list of {@link com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument} by provided
     * UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity and coverts them to
     * {@link CarrierReferenceDocumentDto} entities.
     *
     * @param shipmentId - UUID of {@link com.sap.gtt.v2.sample.sst.odata.model.Shipment} entity
     * @return list of {@link CarrierReferenceDocumentDto} entities
     */
    List<CarrierReferenceDocumentDto> getByShipmentId(@NotNull final String shipmentId);
}
