package com.sap.gtt.v2.sample.sst.rest.model.converter;

import static org.assertj.core.api.Assertions.assertThat;

import com.sap.gtt.v2.sample.sst.common.utils.ODataUtils;
import com.sap.gtt.v2.sample.sst.common.utils.SSTUtils;
import com.sap.gtt.v2.sample.sst.odata.model.CarrierReferenceDocument;
import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import java.util.List;
import org.junit.jupiter.api.Test;

class CarrierReferenceDocumentConverterTest {

    private final CarrierReferenceDocumentConverter carrierReferenceDocumentConverter =
            new CarrierReferenceDocumentConverter();

    @Test
    void toDto_givenCarrierRefDocument_shouldReturnDto() {
        // given
        final String carrierRefDocumentsJson = SSTUtils.getStringFromResource("/odata/carrier-ref-documents.json");
        final CarrierReferenceDocument carrierReferenceDocument =
                ODataUtils.readEntitySet(carrierRefDocumentsJson, CarrierReferenceDocument.class).getResults().get(0);

        // when
        final CarrierReferenceDocumentDto dto = carrierReferenceDocumentConverter.toDto(carrierReferenceDocument);

        // then
        assertThat(dto).isNotNull();
    }

    @Test
    void toDtoList() {
        // given
        final String carrierRefDocumentsJson = SSTUtils.getStringFromResource("/odata/carrier-ref-documents.json");
        final List<CarrierReferenceDocument> carrierReferenceDocuments =
                ODataUtils.readEntitySet(carrierRefDocumentsJson, CarrierReferenceDocument.class).getResults();

        // when
        final List<CarrierReferenceDocumentDto> dtos = carrierReferenceDocumentConverter.toDtoList(carrierReferenceDocuments);

        // then
        assertThat(dtos).isNotEmpty();
    }
}
