package com.sap.gtt.v2.sample.sst.rest.service;

import static com.sap.gtt.v2.sample.sst.common.utils.SSTUtils.getStringFromResource;
import static java.util.UUID.randomUUID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.contains;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.springframework.http.HttpMethod.GET;

import com.sap.gtt.v2.sample.sst.ITCommonAbstract;
import com.sap.gtt.v2.sample.sst.rest.model.dto.CarrierReferenceDocumentDto;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;

class ITCarrierReferenceDocumentServiceImpl extends ITCommonAbstract {

    @Autowired
    private CarrierReferenceDocumentService carrierReferenceDocumentService;

    @Test
    void getByShipmentId_givenShipmentId_shouldReturnReferenceDocuments() {
        // given
        final String shipmentId = randomUUID().toString();
        final String shipmentJson = getStringFromResource("/odata/shipment.json");

        doReturn(ResponseEntity.ok().body(shipmentJson))
                .when(restTemplate)
                .exchange(contains("/Shipment"), eq(GET), any(HttpEntity.class), eq(String.class));

        // when
        final List<CarrierReferenceDocumentDto> referenceDocumentDtos = carrierReferenceDocumentService.getByShipmentId(shipmentId);

        // then
        assertThat(referenceDocumentDtos).hasSize(1);
    }
}
