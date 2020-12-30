package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static java.util.stream.Collectors.toList;

import com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument;
import com.sap.gtt.v2.sample.sst.rest.model.ActualSpot;
import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

/**
 * {@link CarrierReferenceDocumentConverter} is a converter which converts provided entities to
 * {@link CarrierReferenceDocumentDto}.
 *
 * @author Aliaksandr Miron
 */
@Validated
@Component
public class CarrierReferenceDocumentConverter {

    /**
     * Converts provided {@link CarrierReferenceDocument} entity to {@link CarrierReferenceDocumentDto} entity.
     *
     * @param carrierReferenceDocument - {@link CarrierReferenceDocument} entity to be converted
     * @return {@link CarrierReferenceDocumentDto} entity
     */
    public CarrierReferenceDocumentDto toDto(@NotNull final CarrierReferenceDocument carrierReferenceDocument) {
        final CarrierReferenceDocumentDto dto = new CarrierReferenceDocumentDto();
        dto.setDocId(carrierReferenceDocument.getDocId());
        dto.setDocTypeCode(carrierReferenceDocument.getDocTypeCode());
        return dto;
    }

    /**
     * Converts provided list of {@link CarrierReferenceDocument} entities to list of
     * {@link CarrierReferenceDocumentDto} entities.
     *
     * @param carrierReferenceDocuments list of {@link CarrierReferenceDocument} to be converted
     * @return list of {@link ActualSpot}
     */
    public List<CarrierReferenceDocumentDto> toDtoList(
            @NotNull final List<CarrierReferenceDocument> carrierReferenceDocuments) {
        return carrierReferenceDocuments.stream().map(this::toDto).collect(toList());
    }
}
